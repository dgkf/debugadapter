debuggee <- R6::R6Class(
  "debuggee",
  inherit = client,
  public = list(
    #' @field breakpoints currently tracked breakpoints
    breakpoints = list(),

    #' @field calls During a debugging state, the calls in the debugging 
    #'   call stack.
    calls = list(),

    #' @field frames During a debugging state, the frames in the debugging
    #'   call stack.
    frames = list(),

    #' @field stack_frames the most recent stopping event's stack frames
    stack_frames = list(),

    initialize = function(...) {
      # the 'client' we're connected to is the adapter
      self$arguments <- list(clientName = "adapter")
      super$initialize(...)
    },

    get_stack_frames = function() {
      stack_frames(self$calls, self$frames)
    },

    get_scopes = function(id) {
      get_scopes(self$frames, id)
    },

    get_variables = function(varref) {
      # for now, varref is always the frame id
      get_variables(self$frames[[varref]])
    },

    handle = function(x, ...) r_handle(x, ..., debuggee = self)
  )
)

stack_frames <- function(calls, frames) {
  mfapply(
    function(id, call, frame) {
      # find source object (which may be different that a traced object)
      obj <- tryCatch(eval(call[[1]], envir = frame), error = function(e) NULL)
      what <- find_source_object(obj)

      # derive some source information 
      filepath <- getSrcFilename(what, full.names = TRUE)
      span <- getSrcref(what)
      if (!is.null(span)) span <- as.numeric(span)

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
        # moduleId = package name if possible
      )
    },
    seq_along(calls),
    calls,
    frames
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
  bps <- lapply(x$arguments$breakpoints, verify_breakpoint)
  trace_breakpoints(bps)
  write_message(debuggee, response(x, list(breakpoints = bps)))
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
