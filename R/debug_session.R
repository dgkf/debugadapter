  BROWSER_PROMPT_RE <- "Browse\\[(-?\\d+)\\]>\\s*$"
  DONE_SEMAPHORE <- "__dapr_done__"


#' Shadow browser
#'
#' Run `browser()` in a forked process, presenting a debugger in a familiar
#' `browser()` interface, while allowing additional metadata to be captured to
#' feed an IDE using the debug adapter protocol.
#'
#' @inheritParams base::browser
#'
#' @examples
#' x <- function(...) { y(...) }
#' y <- function(...) {
#'   1
#'   2
#'   3
#'   print("hello, world!")
#' }
#'
#' x()
#'
#' @export
shadow_browser <- function(...) {
  pipe <- shadow_browser_pipes()

  # in the parent process, redirect all output to child
  orig_stdin  <- processx::conn_set_stdin(pipe$from_child, drop = FALSE)
  orig_stdout <- processx::conn_set_stdout(pipe$to_child, drop = FALSE)

  f <- parallel:::mcfork()
  if (inherits(f, "masterProcess")) {
    on.exit({
      lapply(pipe, close)
      parallel:::mcexit(1L, "Error occurred in shadow browser.")
    })

    input <- ""
    repeat {
      # read our first message, the initial prompt
      msg <- read_until_browse_prompt(pipe$from_parent)

      # present cleaner stdout input to user
      msg <- trimws(msg, "left")
      if (startsWith(msg, input)) msg <- trimws(substring(msg, nchar(input) + 1), "left")
      message("\r", msg, appendLF = FALSE)
      if (isTRUE(attr(msg, "done"))) break

      debug <- step_shadow_browser(pipe$to_parent, pipe$from_parent, pipe$read_debug)
      if (isTRUE(debug$finished)) break

      # TODO: handle multiline input
      input <- processx::conn_read_lines(orig_stdin, n = 1)
      processx::conn_write(pipe$to_parent, paste0(input, "\n"))
    }

    lapply(pipe, close)
    parallel:::mcexit(0L)
  }

  # revert our stdin/stdout once we've returned to top level repl
  addTaskCallback(name = "close shadow browser", function(...) {
    # for some reason callbacks are called within browser at lower stack depth?
    still_browsing <- sys.nframe() > 1

    if (!still_browsing) {
      processx::conn_set_stdin(orig_stdin)
      processx::conn_set_stdout(orig_stdout)

      # signal that we're done and close all pipes
      processx::conn_write(pipe$to_child, DONE_SEMAPHORE)
      lapply(pipe, close)
    }

    still_browsing
  })

  parent <- parent.frame()
  parent$.dapr_con <- pipe$write_debug

  expr <- substitute(browser(...))
  eval.parent(expr)
}



#' Create a pair of pipe pairs for bi-directional communication
#'
shadow_browser_pipes <- function() {
  # parent stdin --> child stdin
  to_child  <- processx::conn_create_pipepair()

  # child stdout --> parent stdout
  to_parent <- processx::conn_create_pipepair()

  # child debug mesages --> DAP
  debug     <- processx::conn_create_pipepair()

  list(
    to_child = to_child[[1]],
    to_parent = to_parent[[1]],
    from_child = to_parent[[2]],
    from_parent = to_child[[2]],
    write_debug = debug[[1]],
    read_debug = debug[[2]]
  )
}



step_shadow_browser <- function(to, from, debug, clear = TRUE) {
  # query debug state
  processx::conn_write(to, "dapr:::sync_shadow_browser_state(.dapr_con)\n")
  debug <- read_message(debug, level = TRACE)

  # flush browser output from querying debugger state
  read_until(from, BROWSER_PROMPT_RE)

  debug
}


sync_shadow_browser_state <- function(con) {
  # DAP code here. Communicate with child to provide
  #   - stackFrame
  #   - environment vars

  write_message(con, list(finished = FALSE), level = TRACE)
  TRUE
}

read_until_browse_prompt <- function(con, ...) {
  read_until(con, BROWSER_PROMPT_RE, ...)
}

read_until <- function(con, x, ..., sleep = 0.05) {
  res <- ""
  repeat {
    res_next <- processx::conn_read_chars(con)

    if (grepl(DONE_SEMAPHORE, res_next, fixed = TRUE)) {
      res <- trimws(paste0(res, gsub(paste0(DONE_SEMAPHORE, ".*"), "", res_next)), "left")
      attr(res, "done") <- TRUE
      return(res)
    }

    res <- paste0(res, res_next)
    if (grepl(x, res, ...)) break
    Sys.sleep(sleep)
  }

  res
}

# replicate utils::setBreakpoint, with our own browser function
set_breakpoint <- function() { }
formals(set_breakpoint) <- formals(utils::setBreakpoint)
body(set_breakpoint) <- {
  eval(bquote(
    substitute(
      .(body(utils::setBreakpoint)),
      list(browser = as.symbol("shadow_browser"))
    )
  ))
}
