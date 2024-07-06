#' Protocol Message Reading and Parsing
#'
#' @param x A connection (or similar) to read from or write to
#' @param content The message contents to write
#' @param timeout A maximum time in seconds to wait to receive a new message
#' @param verbose A logical value or integer log level flag
#' @param name An optional string to use for the connection for debug logs
#'
#' @name messages
NULL

#' @name messages
read_message <- function(x, ...) {
  UseMethod("read_message")
}

#' @export
#' @name messages
read_message.client <- function(x, ...) {
  read_message(x$connection, ..., name = x$name())
}

#' @export
#' @name messages
read_message.default <- function(x, ..., verbose = DEBUG, name = NULL) {
  # read until the next "Content-Length" header
  # (flushing any leading whitespace)
  scan(x, what = character(), n = 1, sep = "C", quiet = TRUE, skipNul = TRUE)

  # read in content length header, and stream in json body
  header <- scan(
    x,
    what = character(), n = 1, sep = "\n", quiet = TRUE, skipNul = TRUE
  )
  content_length <- as.numeric(sub("^C?ontent-Length: ", "", trimws(header)))

  # scan to newline preceeding content body, +2 for leading \r\n
  # NOTE: suppressing warning from readChar when used on a socket connection
  nchars <- content_length + 2L
  body <- suppressWarnings(readChar(x, nchars = nchars, useBytes = TRUE))
  if (length(body) < 1 || nchar(trimws(body)) < 1) {
    return(NULL)
  }

  obj <- parse_message_body(body)

  log_header <- paste0(
    "received (", content_length, ")",
    if (!is.null(name)) paste0(" from ", name), "\n"
  )

  log(verbose, log_header, strip_empty_lines(trimws(body)), "\n")
  obj
}

#' @name messages
write_message <- function(x, content = list(), ...) {
  UseMethod("write_message")
}

#' @export
#' @name messages
write_message.client <- function(x, content = list(), ...) {
  write_message(x$connection, content = content, ..., name = x$name())
}

#' @export
#' @name messages
write_message.default <- function(x, content = list(), verbose = DEBUG, name = NULL) {
  content_str <- format_message_content(content)
  log_header <- paste0(
    "sent (", nchar(content_str), ")",
    if (!is.null(name)) paste0(" to ", name), "\n"
  )

  log_content <- strip_empty_lines(trimws(sub(".*\n\r", "", content_str)))
  log(verbose, log_header, log_content, "\n")
  writeChar(content_str, x)
  content
}

#' @name messages
parse_message_body <- function(content) {
  obj <- jsonlite::parse_json(content, simplifyVector = FALSE)
  attr(obj, "raw") <- sprintf(
    "Content-Length: %.f\r\n%s",
    nchar(content) - 2, content
  )
  class(obj) <- c(obj$type, obj$command, class(obj))
  obj
}

#' @name messages
format_message_content <- function(content) {
  if (!is.character(content)) {
    content <- jsonlite::toJSON(content, auto_unbox = TRUE)
  }

  paste0(
    "Content-Length: ", nchar(content), "\r\n",
    "\r\n",
    content
  )
}
