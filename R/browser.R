#' Generate a Debug Adapter Browser Hook
#'
#' @param connection A connection to a debug adapter. This connection is used
#'   to send information about the active debug state.
#'
#' @export
browser_hook_sync_debugger <- function(debuggee) {
  local({
    # last object debugged
    addr <- NULL

    # last line within object debugged
    line <- 0L

    # list of breakpoint locations within this object
    locations <- list()

    # vector of breakpoint lines within this object
    breakpoints <- integer(0L)

    #' helper to create skip conditions
    skip_browser_step <- function() {
      cond <- simpleCondition("skipping browser step")
      class(cond) <- c("skip_browser_step", class(cond))
      signalCondition(cond)
    }

    #' reset sinks
    reset_sinks <- function(n) {
      while (sink.number() > n) sink()
    }

    #' The initial hook is used only to skip the first browser prompt
    #'
    #' When a `browser()` trace is inserted, it adds an expression just before
    #' our traced expression. This adds an unnecessary step, so we can just 
    #' bypass the first browser hit and re-enter on the next one.
    #'
    function(hook, condition, envir) {
      # attempt to suppress default debug output so we can make it prettier
      nsinks <- sink.number()
      sink(nullfile())

      call <- sys.call(-1L)
      cur_addr <- rlang::obj_address(eval(call[[1]], envir = parent.env(envir)))
      cur_line <- getSrcref(sys.call())[[1]]

      # check to see if we've entered into a new scope
      if (!identical(cur_addr,  addr) || cur_line < line) {
        addr <<- cur_addr
        locations <<- debuggee$traces[[addr]] %||% list()
        breakpoints <<- vnapply(locations, `[[`, "line")
      }
      line <<- cur_line
        
      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              if (!line %in% breakpoints) skip_browser_step()

              # # update some debuggee state
              # # NOTE: this frame count might not be correct as we step into
              # # functions - needs testing
              # n <- nframe - (session[["skipCalls"]] %||% 4L)
              # debuggee$calls <- utils::head(sys.calls(), n)
              # debuggee$frames <- utils::head(sys.frames(), n)

              # # start stopped sequence
              # write_message(debuggee, event("stopped", list(
              #   reason = "breakpoint",
              #   threadId = 0,
              #   allThreadsStopped = TRUE,
              #   hitBreakpointIds = list(session$breakpoint$id)
              # )))

              # # TODO: handle this more gracefully instead of just spamming
              # # connection listening
              # # handle follow-up requests
              # start <- Sys.time()  # manually handle timeout for now
              # while (Sys.time() - start < 0.5) {
              #   if (!is.null(msg <- read_message(debuggee))) {
              #     debuggee$handle(msg)
              #     start <- Sys.time()
              #   }
              #   Sys.sleep(0.01)
              # }

              reset_sinks(nsinks)
              expr <- sys.call()
              cli::cat_line(cli::style_bold(
                "Debugging at ",
                if (!is.null(f <- getSrcFilename(expr))) f,
                "#", getSrcref(expr)[[1]], 
                ": ", capture.output(expr)[[1]]
              ))

              repeat {
                resp <- parse(prompt = cli::col_yellow("‼ "), n = 1)
                if (is.expression(resp)) resp <- resp[[1]]
                if (is.symbol(resp)) switch(as.character(resp),
                  "where" = print(sys.calls()),
                  "n" = {
                    sink(nullfile())
                    skip_browser_step()
                  },
                  "c" = break,
                  print(eval(resp, envir = envir))
                ) else {
                  print(eval(resp, envir = envir))                  
                }
              }
            }, 
            skip_browser_step = function(cond) {
              # TRACE("skipping browser step ", session$n)
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
          # TRACE("initial browser restart")
        }
      )
    }
  })
}
