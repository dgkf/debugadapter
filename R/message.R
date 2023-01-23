read_message <- function(x, ...) {
  UseMethod("read_message")
}

read_message.debug_adapter <- function(x, ...) {
  read_message(x$con, ...)
}

read_message.debugger <- function(x, ...) {
  read_message(x$con, ...)
}

read_message.default <- function(x, ..., level = DEBUG) {
  # read until the next "Content-Length" header (flushing any leading whitespace)
  scan(x, what = character(), n = 1, sep = "C", quiet = TRUE, skipNul = TRUE)

  # read in content length header, and stream in json body
  header <- scan(x, what = character(), n = 1, sep = "\n", quiet = TRUE, skipNul = TRUE)
  content_length <- as.numeric(sub("^C?ontent-Length: ", "", trimws(header)))

  # scan to newline preceeding content body, +2 for leading \r\n
  body <- readChar(x, nchars = content_length + 2L, useBytes = TRUE)
  if (length(body) < 1 || nchar(trimws(body)) < 1) return(NULL)

  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(level, strip_empty_lines(log_msg), "\n")

  obj
}

read_message.processx_connection <- function(x, timeout = Inf, ..., level = DEBUG) {
  x <- read_until(x, "Content-Length: \\d+\\b", timeout = timeout)
  if (is.null(x)) return(x)

  content_str <- gsub(".*Content-Length: (\\d+)\\s.*", "\\1", x)
  content_length <- as.numeric(content_str)

  body <- gsub(".*Content-Length: \\d+", "", x)
  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(level, strip_empty_lines(log_msg), "\n")

  obj
}

write_message <- function(x, content = list(), ...) {
  UseMethod("write_message")
}

write_message.debug_adapter <- function(x, ...) {
  write_message(x$con, ...)
}

write_message.debugger <- function(x, ...) {
  write_message(x$con, ...)
}

write_message.NULL <- function(x, content = list()) {
  content
}

write_message.default <- function(x, content = list(), level = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent (", nchar(content_str), ")\n", trimws(content_str))
  log(level, strip_empty_lines(log_msg), "\n")
  writeChar(content_str, x)
  content
}

write_message.terminal <- function(x, content = list(), level = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent to stdout (", nchar(content_str), ")\n", trimws(content_str))
  log(level, strip_empty_lines(log_msg), "\n")
  writeLines(content_str, x)
  content
}

write_message.processx_connection <- function(con, content = list(), level = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent to socket (", nchar(content_str), ")\n", trimws(content_str))
  log(level, strip_empty_lines(log_msg), "\n")
  processx::conn_write(con, content_str)
  content
}

parse_message_body <- function(msg) {
  obj <- jsonlite::parse_json(msg, simplifyVector = FALSE)
  attr(obj, "raw") <- sprintf("Content-Length: %.f\r\n%s", nchar(msg) - 2, msg)
  class(obj) <- c(obj$type, obj$command, class(obj))
  obj
}

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

echo_responses <- function(res, to) {
  if (is_response(res)) write_message(to, res)
  res
}

flush_messages <- function(from, to, timeout = 0.05) {
  while (is_response(res <- read_message(from, timeout = timeout))) {
    write_message(to, res)
  }
}
