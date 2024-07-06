#' Generate a Debug Adapter Browser Hook
#'
#' @param connection A connection to a debug adapter. This connection is used
#'   to send information about the active debug state.
#'
#' @export
browser_hook_sync_debugger <- function(debuggee) {
  local({
    function(hook, condition, envir) {
      # disallow auto-termination of option
      options(browser.hook = hook)

      # fetch breakpoint location information from trace
      data <- browserCondition()
      location <- data$location
      breakpoint <- data$breakpoint

      # ignore the hook until we exit... otherwise we ifinitely recurse
      hook <- options(browser.hook = NULL)
      on.exit(options(browser.hook = hook, add = TRUE))

      repeat withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              # update some debuggee state
              debuggee$set_stack_frames(sys.calls(), sys.frames())

              # start stopped sequence
              write_message(debuggee, event("stopped", list(
                reason = "breakpoint",
                threadId = 0,
                allThreadsStopped = TRUE,
                hitBreakpointIds = list(breakpoint$id)
              )))

              # handle follow-up requests
              start <- Sys.time()  # manually handle timeout for now
              while (Sys.time() - start < 0.5) {
                if (!is.null(msg <- read_message(debuggee))) {
                  debuggee$handle(msg)
                  start <- Sys.time()
                }
              }

              val <- browser("", condition, envir, 0L)
              return(val)
            },
            finally = cat("[exit browser]\n")
          ),
          error = function(cnd) {
            cat("Error:", conditionMessage(cnd), "\n")
            invokeRestart("browser")
          }
        ),
        browser = function(...) cat("[browser calling handler]\n")
      )
    }
  })
}

