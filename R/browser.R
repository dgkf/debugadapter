#nolint start
BROWSER_PROMPT_RE <- "Browse\\[(?<frame>-?\\d+)\\]>\\s*$"
DONE_SEMAPHORE <- "__debugadapter_done__"
#nolint end



#' Shadow browser
#'
#' Run `browser()` in a forked process, presenting a debugger in a familiar
#' `browser()` interface, while allowing additional metadata to be captured to
#' feed an IDE using the debug adapter protocol.
#'
#' Unfortunately, navigating history with arrows, multiline input and using
#' other terminal-accepted inputs for line navigation will not work.
#'
#' @param ui A formatter for the debug prompt
#' @param con A connection to which state events will be emitted
#' @inheritParams base::browser
#'
#' @examples
#' x <- function() { y() }
#' y <- function() {
#'   shadow_browser(skipCalls = 4)
#'   print("hello, world")
#' }
#'
#' x()
#'
#' @export
shadow_browser <- function(
  ...,
  ui = ui_debugadapter_prompt,
  debugger = getOption("debugadapter.debugger"),
  con = debugger$con %||% processx::conn_create_pipepair()
) {

  pipe <- shadow_browser_pipes()

  # in the parent process, redirect all output to child
  orig_stdin  <- processx::conn_set_stdin(pipe$from_child, drop = FALSE)
  orig_stdout <- processx::conn_set_stdout(pipe$to_child, drop = FALSE)

  # revert our stdin/stdout once we've returned to top level repl
  addTaskCallback(
    name = "close shadow browser",
    function(expr, value, ok, visible) {
      # callbacks are called within browser at lower stack depth?
      still_browsing <- sys.nframe() > 1

      if (!still_browsing) {
        processx::conn_set_stdin(orig_stdin)
        processx::conn_set_stdout(orig_stdout)
        processx::conn_write(pipe$to_child, DONE_SEMAPHORE)
        lapply(pipe, close)
      }

      still_browsing
    }
  )

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

      # remove our input from captured stdin (input + browser prompt)
      msg <- trimws(msg, "left")
      if (startsWith(msg, input))
        msg <- trimws(substring(msg, nchar(input) + 1), "left")

      # parse browser prompt and present debug ui
      message("\r", ui(msg), appendLF = FALSE)
      if (isTRUE(attr(msg, "done"))) break

      debug <- step_shadow_browser(pipe$to_parent, pipe$from_parent, con)
      if (isTRUE(debug$finished)) break

      # TODO: handle multiline input? control characters (probably not)?
      input <- processx::conn_read_lines(orig_stdin, n = 1)
      processx::conn_write(pipe$to_parent, paste0(input, "\n"))
    }

    # spoof a prompt, since we clobbered the prompt on first return to top level
    processx::conn_write(orig_stdin, getOption("prompt"))

    lapply(pipe, close)
    parallel:::mcexit(0L)
  }

  parent <- parent.frame()
  parent$.con <- con

  expr <- substitute(browser(...))
  eval.parent(expr)
}



ui_browser_prompt <- function(x) {
  x
}


ui_debugadapter_prompt <- function(x) {
  if (grepl(BROWSER_PROMPT_RE, x, perl = TRUE)) {
    gsub(BROWSER_PROMPT_RE, "D[\\1]> ", x, perl = TRUE)
  } else {
    x
  }
}



#' Create a pair of pipe pairs for bi-directional communication
#'
shadow_browser_pipes <- function() {
  # parent stdin --> child stdin
  to_child  <- processx::conn_create_pipepair()

  # child stdout --> parent stdout
  to_parent <- processx::conn_create_pipepair()

  list(
    to_child = to_child[[1]],
    to_parent = to_parent[[1]],
    from_child = to_parent[[2]],
    from_parent = to_child[[2]]
  )
}



step_shadow_browser <- function(to, from, .con, clear = TRUE) {
  # query debug state on parent process & await reply before continuing
  processx::conn_write(to, "debugadapter:::sync_shadow_browser_state(.con)\n")
  debug <- read_message(.con, level = TRACE)

  # flush browser output from querying debugger state
  read_until(from, BROWSER_PROMPT_RE)

  debug
}


sync_shadow_browser_state <- function(con) {
  breakpoint <- getOption("debugadapter.current_breakpoint", list())
  msg <- event(stopped(
    reason = "breakpoint",
    description = "Paused on breakpoint",
    allThreadsStopped = TRUE,
    hitBreakpointIds = breakpoint$id
  ))

  # send stopped event to client, await threads request
  write_message(con, msg)
  resp <- read_message(con)

  log(DEBUG, resp)

  # reply to sync request to allow debugger to continue
  write_message(con, list(finished = FALSE), level = TRACE)
  TRUE
}

read_until_browse_prompt <- function(con, ...) {
  read_until(con, BROWSER_PROMPT_RE, ...)
}

read_until <- function(con, x, ..., sleep = 0.05, timeout = Inf) {
  res <- ""

  start <- Sys.time()
  while (nchar(res) > 0 || Sys.time() - start < timeout) {
    res_next <- processx::conn_read_chars(con)
    if (grepl(DONE_SEMAPHORE, res_next, fixed = TRUE)) {
      res <- trimws(paste0(res, gsub(paste0(DONE_SEMAPHORE, ".*"), "", res_next)), "left")
      attr(res, "done") <- TRUE
      return(res)
    }

    res <- paste0(res, res_next)
    if (grepl(x, res, ..., perl = TRUE)) break
    Sys.sleep(sleep)
  }

  if (nchar(res) == 0) return(NULL)
  res
}
