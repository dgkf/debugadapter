#' Debugger Interfaces
#'
#' These classes are stubs used for dispatching on choice of debugger
#' interfaces. How the adapter interacts with clients can vary based on the
#' client that is used.
#'
#' @name debugger-interfaces
NULL



debugger <- function(con = NULL, subclasses = c()) {
  fields <- as.environment(list(
    con = con,
    breakpoints = list()
  ))

  structure(
    list(),
    state = fields,
    class = c(subclasses, "debugger", "stateful_struct")
  )
}



debugger_handle <- function(x, ...) {
  UseMethod("debugger_handle")
}

debugger_handle.debug_adapter <- function(x, ...) {
  UseMethod("debugger_handle", x$debugger)
}

debugger_handle.default <- function(x, ...) {
  FALSE
}



#' @describeIn debugger-interfaces
#' Debugging commands are sent to a terminal managed by the client.
#' @export
debug_in_client_terminal <- function(con) {
  debugger(con, "debugger_client_terminal")
}
