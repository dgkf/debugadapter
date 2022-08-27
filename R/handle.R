#' Dispatch for protocol messaging
#'
#' @rdname protocol-handlers
#'
handle <- function(x, ...) {
  UseMethod("handle")
}

#' @noRd
handle.NULL <- function(x, ...) {
}

#' @noRd
handle.default <- function(x, ...) {
  warning(
    "Don't know how to handle message:\n",
    paste0("  ", capture.output(x), collapse = "\n")
  )
}

#' @noRd
handle.debug_adapter <- function(x, ...) {
  resp <- read_message(x$con)
  handle(resp, adapter = x)
}



#' @noRd
handle.request <- function(x, ...) {
  UseMethod("handle.request")
}

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

#' @describeIn protocol-handlers
#' Recieve attach request from client and respond to confirm.
#' `r spec("#Requests_Attach")`
handle.request.attach <- function(x, ..., adapter) {
  write_message(adapter$con, response(x))
}

#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_setExceptionBreakpoints")`
handle.request.setExceptionBreakpoints <- function(x, ..., adapter) {
  # TODO: use breakpoints
  write_message(adapter$con, response(x, body = list(breakpoints = list())))
}

#' @describeIn protocol-handlers
#' Update internal adapter breakpoints listing and relay breakpoints to debug
#' session.
#' `r spec("#Requests_SetBreakpoints")`
handle.request.setBreakpoints <- function(x, ..., adapter) {
  write_message(adapter$con, response(x, body = debugger_handle(adapter$debugger, x)))
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
