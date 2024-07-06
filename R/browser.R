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
    trace_nframe <- sys.nframe()

    step_hook <- function(hook, condition, envir) {
      TRACE("[in step hook]")

      # re-enable our browser hook for next browser prompt
      on.exit(options(browser.hook = hook, add = TRUE))

      # fetch breakpoint location information, passed via trace condition
      data <- browserCondition()
      location <- data$location
      breakpoint <- data$breakpoint

      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              # NOTE: at one point I was accidentally throwing an error here,
              # but it let me skip the initial browser call and go right to
              # the traced line. Worth exploring in the future to avoid
              # unnecessary "browser" debug statement before hitting the 
              # actual breakpoint expression

              # update some debuggee state
              # NOTE: this frame count might not be correct as we step into 
              # functions - needs testing
              n <- trace_nframe - session$skipCalls
              debuggee$calls <- utils::head(sys.calls(), n)
              debuggee$frames <- utils::head(sys.frames(), n)

              # start stopped sequence
              write_message(debuggee, event("stopped", list(
                reason = "breakpoint",
                threadId = 0,
                allThreadsStopped = TRUE,
                hitBreakpointIds = list(breakpoint$id)
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

              val <- browser("", condition, envir, 0L)
              return(val)
            },
            finally = cat("[exit browser]\n")
          ),
          error = function(cnd) {
            cat("Error:", conditionMessage(cnd), "\n")
            cat(paste0(
              seq_along(sys.calls()), ". ",
              lapply(sys.calls(), deparse, nlines = 1L, width.cutoff = 80),
              collapse = "\n"
            ))
            invokeRestart("browser")
          }
        ),
        browser = function(...) TRACE("[browser calling handler]")
      )
    }

    #' The initial hook is used only to skip the first browser prompt
    #'
    #' When a `browser()` trace is inserted, it adds an expression just before
    #' our traced expression. This adds an unnecessary step, so we can just 
    #' bypass the first browser hit and re-enter on the next one.
    #'
    initial_hook <- function(hook, condition, envir) {
      if (is.null(session)) {
        session <<- condition
        session$n <<- 0
      }

      # update our step counter (10k/day #goals)
      session$n <<- session$n + 1

      TRACE("[entering initial hook]")
      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              # cat("[initial hook]\n")
              if (session$n <= 1) stop()
              # cat("[initial hook, repeat]\n")
            }, 
            error = function(e) {
              if (session$n > 1) cat("Error: ", conditionMessage(e), "\n")
              invokeRestart("browser")
            }
          )
        ),
        browser = function(cond, ...) {
          TRACE("[initial browser restart]")
          options(browser.hook = step_hook)
          return(browser("", condition, envir, 0L))
        }
      )
    }

    initial_hook
  })
}

