debuggee <- R6::R6Class(
  "debuggee",
  inherit = client,
  public = list(
    #' @field breakpoints currently tracked breakpoints
    breakpoints = list(),

    #' @field traces mapping of environment to debug trace information,
    #'   specifying line number and breakpoint id, used for determining
    #'   any breakpoints to stop at for actively debugged functions.
    traces = list(),

    #' @field calls During a debugging state, the calls in the debugging
    #'   call stack.
    calls = list(),

    #' @field frames During a debugging state, the frames in the debugging
    #'   call stack.
    frames = list(),

    #' @field stack_frames the most recent stopping event's stack frames
    stack_frames = list(),

    initialize = function(..., timeout = 5.0) {
      # the 'client' we're connected to is the adapter
      self$arguments <- list(clientName = "adapter")
      super$initialize(new_connection(..., timeout = timeout))

      # make a connection to the adapter, registering this session as debuggee
      write_message(self, request("initialize", list(clientName = "r-session")))
      write_message(self, request("attach", list(clientName = "r-session")))

      self
    },

    set_breakpoints = function(breakpoints) {
      locations <- lapply(breakpoints, breakpoint_locations)
      breakpoints <- mfapply(breakpoint_verify, breakpoints, locations)
      ids <- as.character(vnapply(breakpoints, `[[`, "id"))
      self$breakpoints[ids] <- breakpoints

      # NOTE: this can probably be cleaned up quite a bit... making a lot of
      # assumptions of the data structure here that probably can be better
      # handled by helpers or classes
      for (idx in seq_along(ids)) {
        id <- idx[[idx]]
        self$breakpoints[[id]] <- locations[[idx]]
      }

      # store trace locations, to be accessed when entering a debugger to decide
      # when to break.
      locations <- unlist(locations, recursive = FALSE)
      obj_addrs <- lapply(locations, function(loc) {
        rlang::obj_address(get0(loc$name, loc$env, inherits = FALSE))
      })

      locations_by_addr <- split(locations, obj_addrs)
      self$traces[names(locations_by_addr)] <- locations_by_addr
      for (locations in locations_by_addr) location_trace(locations[[1]])

      breakpoints
    },

    get_stack_frames = function() {
      stack_frames(self$calls, self$frames)
    },

    get_scopes = function(id) {
      get_scopes(self$frames, id)
    },

    get_variables = function(varref) {
      # for now, varref is always the frame id
      if (varref <= length(self$frames)) {
        get_variables(self$frames[[varref]])
      } else {
        list()
      }
    },

    set_stack_frames = function(calls, frames) {
      self$calls <- calls
      self$frames <- frames
    },

    reset_stack_frames = function() {
      self$calls <- list()
      self$frames <- list()
    },

    event_stopped_breakpoint = function(ids) {
      write_message(self, event("stopped", list(
        reason = "breakpoint",
        description = "Hit breakpoint",
        threadId = 0,
        allThreadsStopped = TRUE,
        hitBreakpointIds = ids
      )))
    },

    event_stopped_step = function() {
      write_message(self, event("stopped", list(
        reason = "step",
        description = "Paused on expression",
        threadId = 0,
        allThreadsStopped = TRUE
      )))
    },

    handle = function(x, ...) r_handle(x, ..., debuggee = self)
  )
)

stack_frames <- function(calls, frames) {
  mfapply(
    function(id, call, frame) {
      src <- getSrcref(call)

      if (is.null(src)) {
        obj <- tryCatch(
          eval(call[[1]], envir = frame),
          error = function(e) NULL
        )

        # find source object (which may be different that a traced object)
        src <- getSrcref(find_source_object(obj))
      }

      # derive some source information
      filepath <- getSrcFilename(src, full.names = TRUE)
      span <- as.numeric(src)

      # derive a stack frame to send back to the adapter
      list(
        id = id,
        source = Filter(Negate(is.null), list(
          name = basename(filepath),
          path = filepath
        )),
        line = span[[1]],
        column = span[[2]],
        endLine = span[[3]],
        endColumn = span[[4]],
        name = deparse(call[[1]])[[1]]
        # TODO(protocol): moduleId = package name if possible
      )
    },
    seq_along(calls),
    rev(calls),
    rev(frames)
  )
}

get_scopes <- function(frames, id) {
  frame_idx <- as.numeric(id)

  # TODO(protocol): can return multiple scopes, such as arguments, globals
  locals <- list(
    name = "Locals",
    presentationHint = "locals",
    variablesReference = frame_idx,
    namedVariables = length(names(frame)),
    expensive = FALSE
    # TODO(protocol): additional source-related fields
  )

  list(locals)
}

get_variables <- function(frame) {
  lapply(names(frame), function(varname) {
    var <- frame[[varname]]
    list(
      name = varname,
      value = format_variable(var)
      # TODO(protocol): tons of optional enhancements
    )
  })
}

#' @export
#' @name protocol-handlers
r_handle <- function(x, ..., debuggee) {
  UseMethod("r_handle")
}

#' @export
#' @name protocol-handlers
r_handle.default <- function(x, ..., debuggee) {
  NULL
}

#' @export
#' @name protocol-handlers
r_handle.request <- function(x, ..., debuggee) {
  UseMethod("r_handle.request")
}

#' @export
#' @name protocol-handlers
r_handle.response <- function(x, ..., debuggee) {
  UseMethod("r_handle.response")
}

#' @export
#' @name protocol-handlers
r_handle.response.default <- function(x, ..., debuggee) {
  NULL
}

#' @export
#' @name protocol-handlers
r_handle.request.default <- function(x, ..., debuggee) {
  DEBUG("debuggee received unknown request: ", x$command)
}

#' @export
#' @name protocol-handlers
r_handle.request.scopes <- function(x, ..., debuggee) {
  id <- x$arguments$frameId
  write_message(debuggee, response(x, list(scopes = debuggee$get_scopes(id))))
}

#' @export
#' @name protocol-handlers
r_handle.request.setBreakpoints <- function(x, ..., debuggee) {
  # NOTE: when handled by a debuggee, we can assume the adapter has already
  # preprocessed the setBreakpoints request into pending breakpoints
  breakpoints <- debuggee$set_breakpoints(x$arguments$breakpoints)
  write_message(debuggee, response(x, list(breakpoints = breakpoints)))
}

#' @export
#' @name protocol-handlers
r_handle.request.stackTrace <- function(x, ..., debuggee) {
  # TODO: do not yet consider arguments (threadId, startFrame, levels, format)
  frames <- debuggee$get_stack_frames()
  write_message(debuggee, response(x, list(
    stackFrames = frames,
    totalFrames = length(frames)
  )))
}

#' @export
#' @name protocol-handlers
r_handle.request.variables <- function(x, ..., debuggee) {
  varref <- x$arguments$variablesReference
  write_message(debuggee, response(x, list(
    variables = debuggee$get_variables(varref))
  ))
}
