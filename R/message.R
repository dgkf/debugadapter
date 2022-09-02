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
  log(level, paste0("recieved (", content_length, ")"), trimws(body))
  obj
}

read_message.processx_connection <- function(con, timeout = Inf, ..., level = DEBUG) {
  x <- read_until(con, "Content-Length: \\d+\\b", timeout = timeout)
  if (is.null(x)) return(x)

  content_str <- gsub(".*Content-Length: (\\d+)\\s.*", "\\1", x)
  content_length <- as.numeric(content_str)

  body <- gsub(".*Content-Length: \\d+", "", x)
  obj <- parse_message_body(body)
  log(level, paste0("recieved (", content_length, ")"), trimws(body))
  obj
}

write_message <- function(con, content = list(), ...) {
  UseMethod("write_message")
}

write_message.default <- function(con, content = list(), level = DEBUG) {
  content <- format_message_content(content)
  log(level, paste0("sent (", nchar(content), ")"), trimws(content))
  writeChar(content, con)
}

write_message.processx_connection <- function(con, content = list(), level = DEBUG) {
  content <- format_message_content(content)
  log(level, paste0("sent (", nchar(content), ")"), trimws(content))
  processx::conn_write(con, content)
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
