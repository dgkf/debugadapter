debug_prompt <- R6::R6Class(  # nolint
  "debug_prompt",
  public = list(
    #' @field debuggee A debuggee client or connection, used for communicating
    #'   back to the debug adapter, and thereby back to client editors.
    debuggee = NULL,

    #' @field breakpoints list of breakpoints within the current debug scope
    breakpoint_lines = integer(),

    #' @field locations list of locations within the current debug scope
    locations = list(),

    #' @field last_addr memory address of last object debugged
    last_addr = NULL,

    #' @field last_loc last srcref location debugged
    last_loc = integer(8),

    #' @field stepping logical indicating whether we're stepping by line, or
    #'   should continue to the next breakpoint
    stepping = FALSE,

    initialize = function(debuggee, ...) {
      self$debuggee <- debuggee
      self
    },

    browser_hook = function(hook, condition, envir) {
      insert_terminated_callback(self)
      self$bind_local_breakpoints()

      withRestarts(
        withCallingHandlers(
          tryCatch(
            {
              hit <- self$is_at_breakpoint_line()
              if (!self$stepping && !hit) skip_browser_step()

              # NOTE: this frame count might not be correct as we step into
              # functions - needs testing
              self$debuggee$set_stack_frames(sys.calls(), sys.frames())

              if (hit) {
                ids <- self$get_line_breakpoint_ids()
                self$debuggee$event_stopped_breakpoint(ids)
              } else {
                self$debuggee$event_stopped_step()
              }

              # TODO: handle this more gracefully instead of just spamming
              # connection listening to handle follow-up requests
              #  - IDEA: use custom messages to pre-send responses in anticipation 
              #          of frame and variable requests so that we don't need
              #          to hang here and wait for them.
              while (!is.null(msg <- read_message(self$debuggee, timeout = 0.05))) {
                self$debuggee$handle(msg)
              }

              self$print_breadcrumbs(sys.call())
              repeat {
                resp <- parse(prompt = cli::col_yellow("‼ "), n = 1)
                if (is.expression(resp)) resp <- resp[[1]]
                if (is.symbol(resp)) {
                  switch(as.character(resp),
                    "where" = { 
                      sc <- sys.calls()
                      cat(paste0(
                        seq_along(sc), ". ", 
                        vcapply(lapply(sc, `[[`, 1), deparse, nlines = 1),
                        collapse = "\n"
                      ))
                      next 
                    },
                    "n" = { self$stepping <- TRUE; break },  # nolint
                    "c" = { self$stepping <- FALSE; break }  # nolint
                  )
                }
                print(eval(resp, envir = envir))
              }
            },
            skip_browser_step = function(cond) {
              TRACE("step")
              invokeRestart("browser")
            },
            error = function(e) {
              cat(
                "Error in ", 
                deparse(conditionCall(e), nlines = 1, width.cutoff = 40), "\n", 
                conditionMessage(e), "\n"
              )
            },
            finally = function(...) TRACE("debug prompt exiting")
          )
        ),
        browser = function(...) TRACE("debug prompt restart")
      )
    },

    bind_local_breakpoints = function(skip = 1L) {
      envir <- sys.frame(-1 - skip)
      what <- sys.call(-1 - skip)[[1]]
      addr <- rlang::obj_address(eval(what, envir = parent.env(envir)))
      loc <- getSrcref(sys.call(-skip))

      # check to see if we've entered into a new scope, update relevant
      # breakpoint data
      is_new_obj <- !identical(addr, self$last_addr)
      if (is_new_obj) {
        self$last_addr <- addr
        self$locations <- self$debuggee$traces[[addr]] %||% list()
        self$breakpoint_lines <- vnapply(self$locations, `[[`, "line")
      }

      self$last_loc <- loc
    },

    is_at_breakpoint_line = function() {
      self$last_loc[[1]] %in% self$breakpoint_lines
    },

    get_line_breakpoint_ids = function() {
      line <- self$last_loc[[1]]
      locs <- self$locations[which(self$breakpoint_lines == line)]
      lapply(locs, `[[`, "id")
    },

    print_breadcrumbs = function(expr) {
      path <- simple_path(getSrcFilename(expr, full.names = TRUE))
      cli::cat_line(
        "debug at ",
        cli::style_bold(if (!is.null(path)) path),
        cli::style_dim("#"),
        cli::style_bold(getSrcref(expr)[[1]]),
        cli::style_dim(": "),
        cli::style_dim(capture.output(expr)[[1]])
      )
    }
  )
)

#' helper to create skip conditions
skip_browser_step <- function() {
  cond <- simpleCondition("skipping browser step")
  class(cond) <- c("skip_browser_step", class(cond))
  signalCondition(cond)
}

#' helper to manage exit message
insert_terminated_callback <- function(prompt) {
  name <- paste0(packageName(), "_browser_top_level_callback")
  if (!name %in% getTaskCallbackNames()) {
    addTaskCallback(name = name, function(...) {
      prompt$debuggee$reset_stack_frames()
      write_message(prompt$debuggee, event("terminated"))
      FALSE
    })
  }
}
