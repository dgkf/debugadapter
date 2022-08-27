set_breakpoints <- function(adapter, path, line) {
  new <- cbind(path = path, line = line)
  new_keys <- breakpoint_key(path, line)

  removed <- !rownames(adapter$breakpoints) %in% new_keys
  added   <- !new_keys %in% rownames(adapter$breakpoints)

  debugger_set_breakpoints(
    adapter$debugger,
    add = new[added, , drop = FALSE],
    remove = adapter$breakpoints[removed, , drop = FALSE]
  )

  adapter$breakpoints <- remove_stored_breakpoints(adapter$breakpoints, path, line)
  adapter$breakpoints <- append_stored_breakpoints(adapter$breakpoints, path, line)
  adapter$breakpoints
}

remove_stored_breakpoints <- function(breakpoints, path, line) {
  if (missing(line)) {
    remove <- !breakpoints[, "path"] %in% path
  } else {
    keys <- breakpoint_key(path, line)
    remove <- !rownames(breakpoints) %in% keys
    }
  breakpoints[remove, ]
}

append_stored_breakpoints <- function(breakpoints, path, line) {
  new <- cbind(path = path, line = line)
  rownames(new) <- breakpoint_key(path, line)
  breakpoints <- rbind(breakpoints, new)
  breakpoints[!duplicated(breakpoints), ]
}

init_breakpoints <- function() {
  cbind(path = "path", line = "line")[c(),]
}

breakpoint_key <- function(path, line) {
  if (missing(line) && is.matrix(path)) {
    return(breakpoint_key(path[, "path"], path[, "line"]))
  }

  if (length(path) < 1 || length(line) < 1) {
    return(character(0L))
  }

  paste0(path, ":", line)
}

breakpoint_from_key <- function(x) {
  split_at <- regexpr(":(?=\\d$)", x, perl = TRUE)
  paths <- substring(x, 1, split_at - 1)
  lines <- substring(x, split_at + 1, nchar(x))
  cbind(path = paths, line = lines)
}

call_from_breakpoint <- function(x) {
  if (is.vector(x) && is.character(x))
    x <- breakpoint_from_key(x)

  if (!isTRUE(nrow(x) > 0))
    return(list())

  lapply(seq_len(nrow(x)), function(i) {
    package <- find_package_name(x[[i, "path"]])
    call <- bquote(utils::setBreakpoint(
      srcfile = .(x[[i, "path"]]),
      line = .(as.numeric(x[[i, "line"]]))
    ))

    if (!is.null(package))
      call$envir <- bquote(getNamespace(.(package)))

    call
  })
}

setBreakpoints_path <- function(request) {
  request$arguments$source$path
}

setBreakpoints_lines <- function(request) {
  request$arguments$lines
}
