#' Debug Adapter
#'
#' A simple class which stores state for a debug adapter. Operationally, this is
#' an environment for in-place mutation of the adapter state.
#'
#' @param x A connection object used to communicate with the debug client
#'
#' @keywords internal
debug_adapter <- function(x) {
  structure(
    list(),
    state = structure(
      as.environment(list(
        con = x,
        breakpoints = list(),
        client = list(),
        debugger = NULL,
        breakpoint_id = 0
      )),
      class = "debug_state"
    ),
    class = "debug_adapter"
  )
}

#' @describeIn debug_adapter
#' Accessors for the debug adapter
`$.debug_adapter` <- function(x, name) {
  attr(x, "state")[[as.character(name)]]
}

#' @describeIn debug_adapter
#' Assignment to stateful components of the debug adapter
`$<-.debug_adapter` <- function(x, name, value) {
  state <- attr(x, "state")
  state[[as.character(name)]] <- value
  x
}

#' @describeIn debug_adapter
#' Generate a unique id number for breakpoints
#' @keywords internal
next_id <- function(adapter, n = 1) {
  seq(
    from = adapter$breakpoint_id + 1,
    to = adapter$breakpoint_id <- adapter$breakpoint_id + n
  )
}

#' @describeIn debug_adapter
#' Test whether adapter supports a capability
#' @keywords internal
supports <- function(x, capability) {
  substring(capability, 1, 1) <- toupper(substring(capability, 1, 1))
  isTRUE(x$client[[paste0("supports", capability)]])
}

#' @describeIn debug_adapter
#' Convert a debug adapter to a string representation
#' @keywords internal
format.debug_adapter <- function(x) {
  s <- attr(x, "state")
  class(x) <- setdiff(class(x), "debug_adapter")
  paste0(
    "<debug adapter>\n",
    paste0(capture.output(x), collapse = "\n"),
    paste0(
      vcapply(names(s), function(n) paste0(n, ":\n", capture.output(s[[n]]))),
      collapse = "\n"
    )
  )
}
