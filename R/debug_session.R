  BROWSER_PROMPT_RE <- "Browse\\[(-?\\d+)\\]>\\s*$"


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
shadow_browser <- function(..., envir = parent.frame()) {
  pipe <- shadow_browser_pipes()
  on.exit(lapply(pipe, close))

  f <- parallel:::mcfork()
  if (inherits(f, "masterProcess")) {
    on.exit(parallel:::mcexit(1L, "Error occurred in shadow browser."))

    # set our stdin to use pipe from parent
    processx::conn_set_stdin(pipe$from_parent)
    processx::conn_set_stdout(pipe$to_parent)

    # 'con' used as the write end of a pipe to write back debugger messages
    par <- parent.frame()
    par$.dapr_con <- .dapr_con <- pipe$write_debug

    expr <- substitute(browser(...))
    eval.parent(expr)

    # prompt used as semaphore, needed to keep in-step
    cat("\nBrowse[-1]> ")

    parallel:::mcexit(0L)
  }

  repeat {
    msg <- read_until_browse_prompt(pipe$from_child)
    if (isTRUE(attr(msg, "frame") < 0)) break

    debug <- step_shadow_browser(pipe$to_child, pipe$from_child, pipe$read_debug)
    if (isTRUE(debug$finished)) break

    cat(msg)
    x <- readline(prompt = "")
    processx::conn_write(pipe$to_child, paste0(x, "\n"))
  }

  invisible(NULL)
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
  res <- list(stdout = "", debug = list())

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
  msg <- read_until(con, BROWSER_PROMPT_RE, ...)
  frame <- as.numeric(gsub(paste0(".*", BROWSER_PROMPT_RE, ".*"), "\\1", msg))
  attr(msg, "frame") <- frame
  msg
}

read_until <- function(con, x, ..., sleep = 0.05) {
  res <- ""
  repeat {
    res_next <- processx::conn_read_chars(con)
    res <- paste0(res, res_next)
    if (grepl(x, res, ...)) break
    Sys.sleep(sleep)
  }
  res
}

x <- function(...) {
  y(...)
}

y <- function(...) {
  {
    dapr:::shadow_browser(skipCalls = 1)
    1
  }
  2
  3
  print("hello, world!")
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
