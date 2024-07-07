#' Generate a Debug Adapter Browser Hook
#'
#' @param connection A connection to a debug adapter. This connection is used
#'   to send information about the active debug state.
#'
#' @export
browser_hook_sync_debugger <- function(debuggee) {
  local({
    #' debug session state
    session <- NULL
    nframe <- sys.nframe()

    #' helper to create skip conditions
    skip_browser_step <- function() {
      cond <- simpleCondition("skipping browser step")
      class(cond) = c("skip_browser_step", class(cond))
      signalCondition(cond)
    }

    #' The initial hook is used only to skip the first browser prompt
    #'
    #' When a `browser()` trace is inserted, it adds an expression just before
    #' our traced expression. This adds an unnecessary step, so we can just 
    #' bypass the first browser hit and re-enter on the next one.
    #'
    function(hook, condition, envir) {
      # condition is null for subsequent browser calls, only initialize session
      # on first run - **requires that a condition is used in browser() call**
      if (!is.null(condition)) {
        session <<- condition
        session$n <<- -(condition[["skip"]] %||% 1)
      }

      # count our steps (trying to hit 10k/day)
      session$n <<- session$n + 1

      TRACE("entering browser")
      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              if (session$n <= 0) skip_browser_step()

              # update some debuggee state
              # NOTE: this frame count might not be correct as we step into 
              # functions - needs testing
              n <- nframe - session[["skipCalls"]]
              debuggee$calls <- utils::head(sys.calls(), n)
              debuggee$frames <- utils::head(sys.frames(), n)

              # start stopped sequence
              write_message(debuggee, event("stopped", list(
                reason = "breakpoint",
                threadId = 0,
                allThreadsStopped = TRUE,
                hitBreakpointIds = list(session$breakpoint$id)
              )))

              # TODO: handle this more gracefully instead of just spamming
              # connection listening
              # handle follow-up requests
              start <- Sys.time()  # manually handle timeout for now
              while (Sys.time() - start < 0.5) {
                if (!is.null(msg <- read_message(debuggee))) {
                  debuggee$handle(msg)
                  start <- Sys.time()
                }
                Sys.sleep(0.01)
              }

              # ignoreHook avoids the introduction of another browser scope
              browser("", condition, envir, 0L, ignoreHook = TRUE)
            }, 
            skip_browser_step = function(cond) {
              TRACE("skipping browser step ", session$n)
              invokeRestart("browser")
            },
            error = function(e) {
              cat("Error: ", conditionMessage(e), "\n")
            },
            finally = function(...) {
              TRACE("exiting browser")
            }
          )
        ),
        browser = function(cond, ...) {
          TRACE("initial browser restart")
        }
      )
    }
  })
}
