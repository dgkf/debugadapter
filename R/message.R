#' Protocol Message Reading and Parsing
#'
#' @param con A connection to read from or write to
#' @param content The message contents to write
#' @param timeout A maximum time in seconds to wait to receive a new message
#' @param verbose A logical value or integer log level flag
#'
#' @name messages
NULL

#' @name messages
read_message <- function(con, ...) {
  UseMethod("read_message")
}

#' @export
#' @name messages
read_message.default <- function(con, ..., verbose = DEBUG) {
  # read until the next "Content-Length" header
  # (flushing any leading whitespace)
  scan(con, what = character(), n = 1, sep = "C", quiet = TRUE, skipNul = TRUE)

  # read in content length header, and stream in json body
  header <- scan(
    con,
    what = character(), n = 1, sep = "\n", quiet = TRUE, skipNul = TRUE
  )
  content_length <- as.numeric(sub("^C?ontent-Length: ", "", trimws(header)))

  # scan to newline preceeding content body, +2 for leading \r\n
  body <- readChar(con, nchars = content_length + 2L, useBytes = TRUE)
  if (length(body) < 1 || nchar(trimws(body)) < 1) {
    return(NULL)
  }

  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(verbose, strip_empty_lines(log_msg), "\n")

  obj
}

#' @export
#' @name messages
read_message.processx_connection <- function(
    con,
    timeout = Inf,
    ...,
    verbose = DEBUG) {
  x <- read_until(con, "Content-Length: \\d+\\b", timeout = timeout)
  if (is.null(x)) {
    return(x)
  }

  content_str <- gsub(".*Content-Length: (\\d+)\\s.*", "\\1", x)
  content_length <- as.numeric(content_str)

  body <- gsub(".*Content-Length: \\d+", "", x)
  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(verbose, strip_empty_lines(log_msg), "\n")

  obj
}

#' @name messages
write_message <- function(con, content = list(), ...) {
  UseMethod("write_message")
}

#' @export
#' @name messages
write_message.default <- function(con, content = list(), verbose = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent (", nchar(content_str), ")\n", trimws(content_str))
  log(verbose, strip_empty_lines(log_msg), "\n")
  writeChar(content_str, con)
  content
}

#' @export
#' @name messages
write_message.terminal <- function(con, content = list(), verbose = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0(
    "sent to stdout (", nchar(content_str), ")\n",
    trimws(content_str)
  )
  log(verbose, strip_empty_lines(log_msg), "\n")
  writeLines(content_str, con)
  content
}

#' @export
#' @name messages
write_message.processx_connection <- function(
    con,
    content = list(),
    verbose = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0(
    "sent to socket (", nchar(content_str), ")\n",
    trimws(content_str)
  )
  log(verbose, strip_empty_lines(log_msg), "\n")
  processx::conn_write(con, content_str)
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
