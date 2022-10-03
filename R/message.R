read_message <- function(con, ...) {
  UseMethod("read_message")
}

read_message.default <- function(con, ..., level = DEBUG) {
  # read until the next "Content-Length" header (flushing any leading whitespace)
  scan(con, what = character(), n = 1, sep = "C", quiet = TRUE)

  # read in content length header, and stream in json body
  header <- scan(con, what = character(), n = 1, sep = "\n", quiet = TRUE)
  content_length <- as.numeric(sub("^C?ontent-Length: ", "", trimws(header)))

  # scan to newline preceeding content body, +2 for leading \r\n
  body <- readChar(con, nchars = content_length + 2L, useBytes = TRUE)
  if (length(body) < 1 || nchar(trimws(body)) < 1) return(NULL)

  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(level, strip_empty_lines(log_msg), "\n")

  obj
}

read_message.processx_connection <- function(con, timeout = Inf, ..., level = DEBUG) {
  x <- read_until(con, "Content-Length: \\d+\\b", timeout = timeout)
  if (is.null(x)) return(x)

  content_str <- gsub(".*Content-Length: (\\d+)\\s.*", "\\1", x)
  content_length <- as.numeric(content_str)

  body <- gsub(".*Content-Length: \\d+", "", x)
  obj <- parse_message_body(body)

  log_msg <- paste0("recieved (", content_length, ")\n", trimws(body))
  log(level, strip_empty_lines(log_msg), "\n")

  obj
}

write_message <- function(con, content = list(), ...) {
  UseMethod("write_message")
}

write_message.default <- function(con, content = list(), level = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent (", nchar(content_str), ")\n", trimws(content_str))
  log(level, strip_empty_lines(log_msg), "\n")
  writeChar(content_str, con)
  content
}

write_message.terminal <- function(con, content = list(), level = DEBUG) {
  content_str <- format_message_content(content)
  log_msg <- paste0("sent to stdout (", nchar(content_str), ")\n", trimws(content_str))
  log(level, strip_empty_lines(log_msg), "\n")
  writeLines(content_str, con)
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
