TRACE <- 1
DEBUG <- 2
INFO <- 3

log <- function(level, ..., verbose = DEBUG) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (is.character(dots[[i]])) next
    dots[[i]] <- paste0(capture.output(dots[[i]]), "\n")
  }

  if (is.logical(verbose))
    verbose <- if (verbose) 2 else 4

  level_msg <- switch(
    level,
    "1" = "[TRACE] ",
    "2" = "[DEBUG] ",
    "3" = "[INFO] ",
    ""
  )

  if (verbose <= level)
    message(sprintf("%s%s\n", level_msg, paste0(dots, collapse = "\n")))
}

echo <- function(level, msg, verbose = DEBUG) {
  if (verbose <= level) message(msg)
}
