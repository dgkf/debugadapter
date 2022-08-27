debug_adapter <- function(x) {
  structure(
    list(),
    state = structure(
      as.environment(list(
        con = x,
        breakpoints = init_breakpoints(),
        client = list(),
        debugger = NULL
      )),
      class = "debug_state"
    ),
    class = "debug_adapter"
  )
}

`$.debug_adapter` <- function(x, name) {
  attr(x, "state")[[as.character(name)]]
}

`$<-.debug_adapter` <- function(x, name, value) {
  state <- attr(x, "state")
  state[[as.character(name)]] <- value
  x
}

supports <- function(x, capability) {
  substring(capability, 1, 1) <- toupper(substring(capability, 1, 1))
  isTRUE(x$client[[paste0("supports", capability)]])
}

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
