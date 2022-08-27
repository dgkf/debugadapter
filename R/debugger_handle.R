debugger_handle <- function(x, msg, ...) {
  UseMethod("debugger_handle")
}



debugger_handle.debugger_client_terminal <- function(x, msg, ...) {
  UseMethod("debugger_handle.debugger_client_terminal", msg)
}

debugger_handle.debugger_client_terminal.setBreakpoints <- function(x, msg, ...) {
  list(breakpoints = list())
  # return { breakpoints: Breakpoints[] }
}



debugger_handle.debugger_stdout_relay <- function(x, msg, ...) {
  cat(attr(msg, "raw"), "\n")
}



debugger_handle.debugger_here <- function(x, msg, ...) {
  UseMethod("debugger_handle.debugger_here", msg)
}

debugger_handle.debugger_here.character <- function(x, msg, ...) {
  if (is.character(msg)) msg <- parse_message_body(msg)
  NextMethod()
}

debugger_handle.debugger_here.setBreakpoints <- function(x, msg, ...) {
  print(msg)
  list(breakpoints = list())
  # return { breakpoints: Breakpoints[] }
}
