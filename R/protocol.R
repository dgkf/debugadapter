response_add_fields <- function(to, body, ...) {
  if (!missing(body)) to$body <- body
  mask <- list(...)

  for (name in names(mask)) {
    to[[name]] <- mask[[name]]
  }

  to
}

response <- function(to, ...) {
  resp <- list(
    type = "response",
    request_seq = to$seq,
    success = TRUE,
    command = to$command
  )

  response_add_fields(resp, ...)
}

is_response <- function(x) {
  is.list(x) && identical(x$type, "response")
}

event <- function(event, ...) {
  resp <- list(type = "event", event = event)
  response_add_fields(resp, ...)
}
