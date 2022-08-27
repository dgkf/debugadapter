#' Debugger Interfaces
#'
#' These classes are stubs used for dispatching on choice of debugger
#' interfaces. How the adapter interacts with clients can vary based on the
#' client that is used.
#'
#' @name debugger-interfaces
NULL

#' @describeIn debugger-interfaces
#' Debugging commands are sent to a terminal managed by the client.
#' @export
debug_in_client_terminal <- function() {
  structure(TRUE, class = c("debugger_client_terminal", "debugger"))
}

#' @describeIn debugger-interfaces
#' Debugging is handled by emitting R code over stdout for another prcoess to
#' pick up and execute to handle debugging. By default, this is used when the
#' debugger is launched in an interactive session, launching the server in the
#' background, which then relays debug callbacks to the parent process.
#' @export
debug_stdout_relay <- function() {
  structure(TRUE, class = c("debugger_stdout_relay", "debugger"))
}

#' @describeIn debugger-interfaces
#' Handle debugging calls locally ('here').
#' @export
debug_here <- function() {
  structure(TRUE, class = c("debugger_here", "debugger"))
}
