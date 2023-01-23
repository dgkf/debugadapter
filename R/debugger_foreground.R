#' @describeIn debugger-interfaces
#' Handle debugging calls locally ('here').
#' @export
debug_in_foreground <- function(con) {
  x <- debugger(con, "debugger_foreground")
  options(debugadapter.debugger = x)
  x
}



debugger_handle.debugger_foreground <- function(x, ..., timeout = 0.05) {
  resp <- read_message(x$con, timeout = timeout)
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

debugger_fg_handle.setBreakpoints <- function(x, resp, ...) {
  x$breakpoints <- lapply(resp$body$breakpoints, as.breakpoint)
  tracer <- quote(debugadapter::shadow_browser(skipCalls = 8L))

  print("HERE")

  log(DEBUG,
    sprintf("setting %.f breakpoints\n%s",
      length(x$breakpoints),
      paste(lapply(x$breakpoints, format), collapse = "\n")
    )
  )

  trace_breakpoints(x$breakpoints, tracer = tracer)
  TRUE
}
