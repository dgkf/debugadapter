client <- R6::R6Class(
  "client",
  public = list(
    #' @field connection A socket connection to the client
    connection = NULL,

    #' @field capabilities client capabilities
    capabilities = list(),

    #' @field arguments Arbitrary data passed with initialize request
    arguments = NULL,

    #' @field last_read_time The last system time where the connection was
    #' read from
    time_last_message = NULL,

    initialize = function(connection) {
      self$connection <- connection
      self$time_last_message <- Sys.time()
      self
    },

    #' Query whether the client supports a particular capability
    supports = function(capability) {
      n <- utils::head(grep(
        paste0("supports?", capability),
        names(self$capabilities),
        value = TRUE,
        ignore.case = TRUE
      ), 1)

      if (length(n) < 1 || !n %in% names(self$capabilities)) {
        DEUBG(sprintf("Cannot find capability '%s'", capability))
        return(FALSE)
      }

      isTRUE(self$capabilities[[n[[1]]]])
    },

    #' return client id, derived from connection id
    id = function() {
      if (is.null(self$connection)) {
        -1
      } else {
        as.numeric(self$connection)
      }
    },

    #' return client name, either a clientName argument or  id
    name = function() {
      if (!is.null(name <- self$arguments$clientName)) {
        name
      } else {
        self$id()
      }
    }
  )
)

#' @export
format.client <- function(x, ...) {
  arguments <- deparse(x$arguments)
  capabilities <- paste(collapse = "\n", sprintf(
    " * %s",
    names(x$capabilities[!vlapply(x$capabilities, identical, FALSE)])
  ))

  sprintf(
    "<client <connection %s>>\n%s%s",
    as.numeric(x$connection),
    if (length(capabilities)) paste0(capabilities, "\n") else "",
    if (!is.null(x$arguments)) paste0(arguments, "\n") else ""
  )
}

#' @export
print.client <- function(x, ...) {
  cat(format(x, ...))
}
