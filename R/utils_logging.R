# nolint start
TRACE <- 1
DEBUG <- 2
INFO <- 3
# nolint end

log <- function(level, ..., verbose = getOption("debugadapter.log", FALSE)) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (is.character(dots[[i]])) next
    dots[[i]] <- paste0(capture.output(dots[[i]]), collapse = "\n")
  }

  if (is.logical(verbose))
    verbose <- if (verbose) 2 else 4

  log_prefix <- getOption("debugadapter.log_prefix", "")

  level_msg <- switch(
    level,
    "1" = "[TRACE] ",
    "2" = "[DEBUG] ",
    "3" = "[INFO] ",
    ""
  )

  if (verbose <= level)
    message(paste0(
      log_prefix,
      level_msg,
      paste0(dots, collapse = "\n")
    ))
}

echo <- function(level, msg, verbose = DEBUG) {
  if (verbose <= level) message(msg)
}
