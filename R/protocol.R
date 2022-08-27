response <- function(to, body, ...) {
  resp <- list(
    type = "response",
    request_seq = to$seq,
    success = TRUE,
    command = to$command
  )

  if (!missing(body)) resp$body <- body
  mask <- list(...)

  for (name in names(mask)) {
    resp[[name]] <- mask[[name]]
  }

  resp
}

event <- function(event, body, ...) {
  resp <- list(
    type = "event",
    event = event
  )

  if (!missing(body)) resp$body <- body
  mask <- list(...)

  for (name in names(mask)) {
    resp[[name]] <- mask[[name]]
  }

  resp
}
