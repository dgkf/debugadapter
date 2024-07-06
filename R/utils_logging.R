#' @name logging
loglevel <- local({
  x <- c("TRACE", "DEBUG", "INFO")
  x <- factor(x, levels = x)
  names(x) <- x
  lapply(x, identity)
})

#' @name logging
TRACE <- function(...) log(loglevel$TRACE, ...)  # nolint
#' @name logging
DEBUG <- function(...) log(loglevel$DEBUG, ...)  # nolint
#' @name logging
INFO <- function(...) log(loglevel$INFO, ...)  # nolint

#' Emit Logging Messages
#'
#' @param level The logging level that the message should be emitted with
#' @param ... Any number of arguments to comprise the logging message. Atomics
#'   are printed as is, and anything else is formatted before printing.
#' @param verbose A verbosity level. When lower than the logging level, the
#'   logging message is ignored.
#'
#' @name logging
#' @import cli
log <- function(level, ..., verbose = getOption("debugadapter.log", FALSE)) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (is.character(dots[[i]])) next
    if (is.numeric(dots[[i]])) next
    dots[[i]] <- paste0(
      paste0(capture.output(dots[[i]]), collapse = "\n"),
      "\n"
    )
  }

  if (is.logical(verbose)) {
    verbose <- if (verbose) 2 else 4
  }

  log_prefix <- getOption("debugadapter.log_prefix", "")
  level_msg <- switch(as.numeric(level),
    "1" = cli::col_magenta("[TRACE] "),
    "2" = cli::col_yellow("[DEBUG] "),
    "3" = cli::col_blue("[INFO] "),
    ""
  )

  body <- paste0(dots, collapse = "")
  header <- sub("\n.*", "", body)
  body <- substring(body, nchar(header) + 1)

  if (as.numeric(verbose) <= as.numeric(level)) {
    message(paste0(
      cli::style_bold(log_prefix, level_msg, header),
      cli::style_dim(trimws(body, which = "right"))
    ))
  }
}

#' Echo Logging Messages
#'
#' @name logging
echo <- function(level, msg, verbose = getOption("debugadapter.log", FALSE)) {
  if (nchar(msg) > 0 && as.numeric(verbose) <= as.numeric(level)) {
    message(trimws(msg, which = "right"))
  }
}
