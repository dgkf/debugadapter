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

    # last line & column within object debugged
    line <- 0L
    column <- 0L

    # list of breakpoint locations within this object
    locations <- list()

    # vector of breakpoint lines within this object
    breakpoints <- integer(0L)

    # flag for when we want to do an individual step, regardless of breakpoint
    stepping <- FALSE

    #' helper to create skip conditions
    skip_browser_step <- function() {
      cond <- simpleCondition("skipping browser step")
      class(cond) <- c("skip_browser_step", class(cond))
      signalCondition(cond)
    }

    #' helper to manage exit message
    insert_terminated_callback <- function() {
      name <- paste0(packageName(), "_browser_top_level_callback")
      if (!name %in% getTaskCallbackNames()) {
        addTaskCallback(name = name, function(...) {
          browser_sink_close()
          debuggee$calls <- list()
          debuggee$frames <- list()
          write_message(debuggee, event("terminated"))
          FALSE
        })
      }
    }

    #' The initial hook is used only to skip the first browser prompt
    #'
    #' When a `browser()` trace is inserted, it adds an expression just before
    #' our traced expression. This adds an unnecessary step, so we can just
    #' bypass the first browser hit and re-enter on the next one.
    #'
    function(hook, condition, envir) {
      insert_terminated_callback()

      call <- sys.call(-1L)
      cur_addr <- rlang::obj_address(eval(call[[1]], envir = parent.env(envir)))
      cur_line <- getSrcref(sys.call())[[1]]
      cur_column <- getSrcref(sys.call())[[2]]

      # check to see if we've entered into a new scope, update relevant
      # breakpoints
      if (!identical(cur_addr,  addr) || cur_line < line) {
        addr <<- cur_addr
        locations <<- debuggee$traces[[addr]] %||% list()
        breakpoints <<- vnapply(locations, `[[`, "line")
      }

      line <<- cur_line
      column <<- cur_column
      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              hit_breakpoint <- line %in% breakpoints
              if (!stepping && !hit_breakpoint) {
                skip_browser_step()
              }

              # update some debuggee state
              # NOTE: this frame count might not be correct as we step into
              # functions - needs testing
              debuggee$calls <- sys.calls()
              debuggee$frames <- sys.frames()

              # start stopped sequence
              if (hit_breakpoint) {
                id <- locations[[which(breakpoints == line)]]$id
                write_message(debuggee, event("stopped", list(
                  reason = "breakpoint",
                  description = "Hit breakpoint",
                  threadId = 0,
                  allThreadsStopped = TRUE,
                  hitBreakpointIds = list(id)
                )))
              } else {
                write_message(debuggee, event("stopped", list(
                  reason = "step",
                  description = "Paused on expression",
                  threadId = 0,
                  allThreadsStopped = TRUE
                )))
              }

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

              expr <- sys.call()
              path <- simple_path(getSrcFilename(expr, full.names = TRUE))
              cli::cat_line(
                "debug at ",
                cli::style_bold(if (!is.null(path)) path),
                cli::style_dim("#"),
                cli::style_bold(getSrcref(expr)[[1]]),
                cli::style_dim(": "),
                cli::style_dim(capture.output(expr)[[1]])
              )

              repeat {
                resp <- parse(prompt = cli::col_yellow("‼ "), n = 1)
                if (is.expression(resp)) resp <- resp[[1]]
                if (is.symbol(resp)) {
                  switch(
                    as.character(resp),
                    "where" = print(sys.calls()),
                    "n" = {
                      stepping <<- TRUE
                      break
                    },
                    "c" = {
                      stepping <<- FALSE
                      break
                    },
                    print(eval(resp, envir = envir))
                  )
                } else {
                  print(eval(resp, envir = envir))
                }
              }
            },
            skip_browser_step = function(cond) {
              TRACE("browser step")
              invokeRestart("browser")
            },
            error = function(e) {
              cat("Error: ", conditionMessage(e), "\n")
            },
            finally = function(...) {
              TRACE("browser exiting")
            }
          )
        ),
        browser = function(cond, ...) {
          TRACE("browser restart")
        }
      )
    }
  })
}
