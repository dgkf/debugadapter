#' Dispatch for protocol messaging
#'
#' @name protocol-handlers
handle <- function(x, ...) {
  UseMethod("handle")
}

#' @export
#' @name protocol-handlers
handle.NULL <- function(x, ...) {
  FALSE
}

#' @export
#' @name protocol-handlers
handle.request <- function(x, ...) {
  UseMethod("handle.request")
}

#' @export
#' @describeIn protocol-handlers
#' Initiailze the debug adapter and store client information for this session.
#' `r spec("#Requests_Initialize")`
handle.request.initialize <- function(x, ..., adapter, client) {
  client$arguments <- x$arguments
  write_message(client, response(x, debug_adapter_capabilities()))
  write_message(client, event("initialized"))
}

#' @export
#' @describeIn protocol-handlers
#' Attach a client's connection as the active session
#' `r spec("#Requests_Attach")`
handle.request.attach <- function(x, ..., adapter, client) {
  # NOTE: not to spec; special handling for attaching active R session debuggee
  if (identical(x$arguments$clientName, "r-session"))
    adapter$set_debuggee(client)
  write_message(client, response(x))
}

#' @export
#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_setExceptionBreakpoints")`
handle.request.setExceptionBreakpoints <- function(x, ..., adapter, client) {  # nolint
  x$arguments$adapter <- adapter
  # TODO: set_exception_breakoints
  # bps <- do.call(set_exception_breakpoints, x$arguments)
  bps <- list()
  write_message(client, response(x, body = list(breakpoints = bps)))
}

#' @export
#' @describeIn protocol-handlers
#' Set function breakpoints
#' `r spec("#Requests_setFunctionBreakpoints")`
handle.request.setFunctionBreakpoints <- function(x, ..., adapter, client) {
  # TODO: set_function_breakoints
  # bps <- do.call(set_function_breakpoints, x$arguments)
  bps <- list()
  write_message(client, response(x, body = list(breakpoints = bps)))
}

#' @export
#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_SetBreakpoints")`
handle.request.setBreakpoints <- function(x, ..., adapter, client) {
  adapter$set_pending_breakpoints(x)
}

#' @export
#' @describeIn protocol-handlers
#' Handle disconnect request to disconnect from the debuggee, end the debug
#' session and shut itself down.
#' `r spec("#Requests_Disconnect")`
handle.request.disconnect <- function(x, ..., adapter, client) {
}

#' @export
#' @describeIn protocol-handlers
#' Handle configuration done requests.
#' `r spec("#Requests_ConfigurationDone")`
handle.request.configurationDone <- function(x, ..., adapter, client) {
  write_message(client)
}



#' @describeIn protocol-handlers
#' Issue reverse request and await response.
#' `r spec("#Reverse_Requests_RunInTerminal")`
handle.reverse_request <- function(x, ..., adapter, client) {
  write_message(client, x)
  handle(adapter)
}



#' @describeIn protocol-handlers
#' Recieve reverse request response.
#' `r spec("#Reverse_Requests_RunInTerminal")`
handle.response <- function(x, ...) {
  # read response body as primary result
  resp <- read_message(x$con)
  obj <- resp$body

  # add additional fields as attributes
  resp$body <- NULL
  attributes(obj) <- resp

  obj
}
