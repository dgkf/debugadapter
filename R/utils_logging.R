# nolint start
TRACE <- 1
DEBUG <- 2
INFO <- 3
# nolint end

#' @import cli
log <- function(level, ..., verbose = getOption("debugadapter.log", FALSE)) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    if (is.character(dots[[i]])) next
    if (is.numeric(dots[[i]])) next
    dots[[i]] <- paste0(paste0(capture.output(dots[[i]]), collapse = "\n"), "\n")
  }

  if (is.logical(verbose)) {
    verbose <- if (verbose) 2 else 4
  }

  log_prefix <- getOption("debugadapter.log_prefix", "")
  level_msg <- switch(level,
    "1" = cli::col_magenta("[TRACE] "),
    "2" = cli::col_yellow("[DEBUG] "),
    "3" = cli::col_blue("[INFO] "),
    ""
  )

  body <- paste0(dots, collapse = "")
  header <- sub("\n.*", "", body)
  body <- substring(body, nchar(header) + 1)

  if (verbose <= level) {
    message(paste0(
      cli::style_bold(log_prefix, level_msg, header),
      cli::style_dim(trimws(body, which = "right"))
    ))
  }
}

echo <- function(level, msg, verbose = DEBUG) {
  if (nchar(msg) > 0 && verbose <= level) {
    message(trimws(msg, which = "right"))
  }
}
