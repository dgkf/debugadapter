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
handle.default <- function(x, ...) {
  warning(
    "Don't know how to handle message:\n",
    paste0("  ", capture.output(x), collapse = "\n")
  )
}

#' @export
#' @name protocol-handlers
handle.debug_adapter <- function(x, timeout = Inf, ...) {
  resp <- read_message(x$con, timeout = timeout)
  handle(resp, adapter = x)
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
handle.request.initialize <- function(x, ..., adapter) {
  adapter$client <- x$arguments

  if (is.null(adapter$debugger)) {
    if (supports(adapter, "runInTerminal")) {
      adapter$debugger <- debug_in_client_terminal()
    }
  }

  write_message(adapter$con, response(x, debug_adapter_capabilities()))
  write_message(adapter$con, event("initialized"))
}

#' @export
#' @describeIn protocol-handlers
#' Recieve attach request from client and respond to confirm.
#' `r spec("#Requests_Attach")`
handle.request.attach <- function(x, ..., adapter) {
  write_message(adapter$con, response(x))
}

#' @export
#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_setExceptionBreakpoints")`
handle.request.setExceptionBreakpoints <- function(x, ..., adapter) {  # nolint
  x$arguments$adapter <- adapter
  # TODO: set_exception_breakoints
  # bps <- do.call(set_exception_breakpoints, x$arguments)
  bps <- list()
  write_message(adapter$con, response(x, body = list(breakpoints = bps)))
}

#' @export
#' @describeIn protocol-handlers
#' Set function breakpoints
#' `r spec("#Requests_setFunctionBreakpoints")`
handle.request.setFunctionBreakpoints <- function(x, ..., adapter) {
  # TODO: set_function_breakoints
  # bps <- do.call(set_function_breakpoints, x$arguments)
  bps <- list()
  write_message(adapter$con, response(x, body = list(breakpoints = bps)))
}

#' @export
#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_SetBreakpoints")`
handle.request.setBreakpoints <- function(x, ..., adapter) {
  x$arguments$adapter <- adapter
  bps <- do.call(set_breakpoints, x$arguments)
  write_message(adapter$con, response(x, body = list(breakpoints = bps)))
}

#' @export
#' @describeIn protocol-handlers
#' Handle disconnect request to disconnect from the debuggee, end the debug
#' session and shut itself down.
#' `r spec("#Requests_Disconnect")`
handle.request.disconnect <- function(x, ..., adapter) {
  write_message(adapter$con)

  if (x$arguments$restart) {
    # TODO: handle restarts
  }

  if (supports(adapter, "TerminateDebuggee") && x$arguments$terminateDebuggee) {
    close(adapter$con)
  }

  if (supports(adapter, "SuspendDebuggee") && x$arguments$suspendDebuggee) {
    # TODO: handle suspend
  }

  TRUE
}

#' @export
#' @describeIn protocol-handlers
#' Handle configuration done requests.
#' `r spec("#Requests_ConfigurationDone")`
handle.request.configurationDone <- function(x, ..., adapter) {
  write_message(adapter$con)
}



#' @describeIn protocol-handlers
#' Issue reverse request and await response.
#' `r spec("#Reverse_Requests_RunInTerminal")`
handle.reverse_request <- function(x, ..., adapter) {
  write_message(adapter$con, x)
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
