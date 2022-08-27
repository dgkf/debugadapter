debugger_set_breakpoints.debugger_stdout_relay <- function(debugger, x) {
  cat(attr(x, "raw"), "\n")
}
