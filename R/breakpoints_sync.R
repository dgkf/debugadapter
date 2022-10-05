sync_breakpoints <- function(adapter) {
  UseMethod("sync_breakpoints", adapter$debugger)
}

sync_breakpoints.debugger_here <- function(adapter) {
  clear_breakpoints(adapter)
  trace_breakpoints(adapter)
}



clear_breakpoints <- function(adapter) {
  UseMethod("clear_breakpoints", adapter$debugger)
}

clear_breakpoints.debugger_here <- function(adapter) {
  for (b in adapter$breakpoints) untrace_breakpoint(b)
}



trace_breakpoints <- function(breakpoints) {
  for (b in breakpoints) trace_breakpoint(b)
}

trace_breakpoint <- function(b) {
  locations <- get_breakpoint_locations(b$source$path, b$line)
  for (location in locations) {
    trace(
      what = location$name,
      signature = location$signature,
      tracer = quote(debugadapter::shadow_browser(skipCalls = 8L)),
      where = location$env,
      at = location$at,
      print = FALSE
    )
  }
}

untrace_breakpoint <- function(b) {
  locations <- get_breakpoint_locations(b$source$path, b$line)
  for (location in locations) {
    untrace(
      what = location$name,
      signature = location$signature,
      where = location$env
    )
  }
}
