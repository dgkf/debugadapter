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



trace_breakpoints <- function(adapter) {
  UseMethod("trace_breakpoints", adapter$debugger)
}

trace_breakpoints.debugger_here <- function(adapter) {
  for (b in adapter$breakpoints) trace_breakpoint(b)
}



trace_breakpoint <- function(b) {
  for (location in b$.data) {
    output <- capture.output(
      trace(
        what = .(location$name),
        signature = .(location$signature),
        tracer = quote(browser()),
        where = .(location$env),
        at = .(location$at),
        print = FALSE
      )
    )

    log(DEBUG, output)
  }
}

untrace_breakpoint <- function(b) {
  for (location in b$.data) {
    output <- capture.output(try(silent = TRUE, untrace(
      what = location$name,
      signature = location$signature,
      where = location$env
    )))

    log(DEBUG, output)
  }
}
