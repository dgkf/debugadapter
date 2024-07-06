debuggee <- R6::R6Class(
  "debuggee",
  inherit = client,
  public = list(
    #' @field breakpoints currently tracked breakpoints
    breakpoints = list(),
    initialize = function(...) {
      # the 'client' we're connected to is the adapter
      self$arguments <- list(clientName = "adapter")
      super$initialize(...)
    },
    handle = function(x, ...) r_handle(x, ..., debuggee = self)
  )
)

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
  log(DEBUG, "debuggee received unknown request: ", x$command)
}

#' @export
#' @name protocol-handlers
r_handle.request.setBreakpoints <- function(x, ..., debuggee) {
  # NOTE: when handled by a debuggee, we can assume the adapter has already
  # preprocessed the setBreakpoints request into pending breakpoints
  bps <- lapply(x$arguments$breakpoints, verify_breakpoint)
  trace_breakpoints(bps)
  write_message(debuggee, response(x, body = list(breakpoints = bps)))
}
