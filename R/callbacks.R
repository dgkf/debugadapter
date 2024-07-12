#' helper to manage exit message
insert_terminated_callback <- function(prompt) {
  name <- paste0(packageName(), "_browser_top_level_callback")
  if (!name %in% getTaskCallbackNames()) {
    prompt$reset()
    addTaskCallback(name = name, function(...) {
      prompt$debuggee$reset_stack_frames()
      write_message(prompt$debuggee, event("stopped", list(reason = "pause")))
      FALSE
    })
  }
}

#' Insert a Task Callback to Synchronize Session
#'
#' The debug adapter is constantly talking to editors in the background, but
#' it can only interject in our R session periodically to query our session
#' for new information.
#'
#' Top-level task callbacks are one of few ways that we can periodically add
#' additional, recurring code to a typical R session.
#'
#' @param debuggee A debug adapter client, and specifically a debuggee - a
#'   privileged client that is used as the source of debug information.
#' @param process Optionally, a process handle so that we can query the
#'   background process running the debug adapter and surface any errors that
#'   it may have encountered. Not strictly necessary, but very helpful.
#'
insert_sync_callback <- function(debuggee, process) {
  addTaskCallback(name = "Synchronize Debugger", function(...) {
    if (!is.null(process)) {
      DEBUG(sprintf(
        "adapter running in subprocess on pid:%.f (%s)",
        process$get_pid(),
        if (process$is_alive()) "alive" else "stopped"
      ))

      echo(loglevel$DEBUG, process$read_error())
      if (!process$is_alive()) {
        close(debuggee$connection)
        tryCatch(
          process$get_result(),
          error = function(e) {
            e$message <- "in debug adapter running as background process"
            show(e)
            return()
          }
        )
      }
    }

    # handle any bg processes relayed back to parent session
    while (!is.null(msg <- read_message(debuggee, timeout = 0.05))) {
      debuggee$handle(msg)
    }

    # keep callback as long as adapter is alive
    if (is.null(process)) TRUE else process$is_alive()
  })
}
