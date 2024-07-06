#' Protocol Messages
#'
#' Constructing and working with protocol messages
#'
#' The debug adapter protocol relies on a few broad classes of messages
#' including requests, responses, events and reverse requests. Each of these
#' has a rigid structure that is adapted for the individual behavior of each
#' message type.
#'
#' @param x `list` an object
#' @param command,event `string` relevant type information. Varies by
#'   message type.
#' @param arguments,body `list` data to attach to the message. Varies by
#'   message type.
#' @param to `request` When issueing a response, the first argument is the
#'   request that is being responded to, used as the basis for communication.
#'
#' @return A `list` matching the respective message structure
#'
#' @name protocol-messages
NULL

#' @name protocol-messages
request <- function(command, arguments) {
  req <- list(type = "request", command = command)
  if (!missing(arguments)) req$arguments <- arguments
  class(req) <- c("reverse_request", command, class(req))
  req
}

#' @name protocol-messages
response <- function(to, ...) {
  resp <- list(
    type = "response",
    request_seq = to$seq,
    success = TRUE,
    command = to$command
  )

  response_add_fields(resp, ...)
}

#' @name protocol-messages
response_add_fields <- function(to, body, ...) {
  if (!missing(body)) to$body <- body
  mask <- list(...)
  for (name in names(mask)) to[[name]] <- mask[[name]]
  to
}

#' @name protocol-messages
is_response <- function(x) {
  is.list(x) && identical(x$type, "response")
}

#' @name protocol-messages
event <- function(event, ...) {
  resp <- list(type = "event", event = event)
  response_add_fields(resp, ...)
}
