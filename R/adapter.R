#' Debug Adapter
#'
#' A simple class which stores state for a debug adapter. Operationally, this is
#' an environment for in-place mutation of the adapter state.
#'
#' @param x A connection object used to communicate with the debug client
#' @keywords internal
debug_adapter <- function(x, ...) {
  structure(list(), state = debug_state(x, ...), class = "debug_adapter")
}

#' @param con An adapter base R connection or processx connection object
#' @param breakpoints A list of breakpoints currently in place
#' @param client A list of capabilities of the client
#' @param debugger A debugger object, used for dispatching behavior based on
#'   debugger type.
#'
#' @describeIn debug_adapter
#' A simple S3 wrapper around an environment used to track debug adapter state.
#'
#' @keywords internal
debug_state <- function(con = NULL, breakpoints = list(), client = list(),
  capabilities = debug_adapter_capabilities(), debugger = NULL) {
  structure(
    as.environment(list(
      con = con,
      breakpoints = breakpoints,
      client = client,
      capabilities = capabilities,
      debugger = debugger,
      breakpoint_id = 0
    )),
    class = "debug_state"
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
  n <- utils::head(grep(
    paste0("supports?", capability),
    names(x$capabilities),
    value = TRUE,
    ignore.case = TRUE
  ), 1)

  if (length(n) < 1 || !n %in% names(x$capabilities)) {
    log(DEBUG, sprintf("Cannot find capability '%s'", capability))
    return(FALSE)
  }

  isTRUE(x$capabilities[[n[[1]]]])
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
