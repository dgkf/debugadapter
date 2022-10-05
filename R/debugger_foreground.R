#' @describeIn debugger-interfaces
#' Handle debugging calls locally ('here').
#' @export
debug_in_foreground <- function(con) {
  structure(con, class = c("debugger_foreground", "debugger", class(con)))
}



debugger_handle.debugger_foreground <- function(x, ..., timeout = 0.05) {
  resp <- read_message(x, timeout = timeout)
  debugger_fg_handle(x, resp, ...)
}



debugger_fg_handle <- function(x, resp, ...) {
  UseMethod("debugger_fg_handle", dispatch_on(resp$command))
}

debugger_fg_handle.NULL <- function(x, resp, ...) {
  FALSE
}

debugger_fg_handle.default <- function(x, resp, ...) {
  TRUE
}

debugger_fg_handle.setExceptionBreakpoints <- function(x, resp, ...) {
  trace_breakpoints(resp$body$breakpoints)
  TRUE
}

debugger_fg_handle.setBreakpoints <- function(x, resp, ...) {
  trace_breakpoints(resp$body$breakpoints)
  TRUE
}
