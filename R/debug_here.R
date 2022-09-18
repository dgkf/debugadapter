.here <- new.env()

#' Local session ("here") global state
#'
#' @export
here <- function() {
  if (!exists("breakpoint_calls", envir = .here))
    .here$breakpoint_calls <- list()
  .here
}

debugger_set_breakpoints.debugger_here <- function(debugger, x) {
  add_calls <- call_from_breakpoint(add)
  remove_calls <- call_from_breakpoint(remove)

  for (i in seq_along(remove)) {
    key <- remove[[i]]
    call <- remove_calls[[i]]
    call$clear <- TRUE
    eval(call)
    .here$breakpoint_calls[[key]] <- NULL
  }

  for (i in seq_along(add)) {
    key <- add[[i]]
    call <- add_calls[[i]]
    eval(call)
    .here$breakpoint_calls[[key]] <- call
  }
}
