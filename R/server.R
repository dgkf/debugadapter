#' Attach the Current Session to a Debug Adapter
#'
#' Connects the current session as the 'attached' session to a debug, adapter.
#' The current session will be used for managing the debugging environment.
#'
#' @return A socket connection to a debug adapter
#'
#' @export
attach_runtime <- function(port = 18721, poll = 0.1, timeout = 0) {
  # make a few attempts to form connection, in case server doesn't start
  start <- Sys.time()
  repeat {
    con <- tryCatch(
      suppressWarnings(socketConnection(port = port)),
      error = function(e) e
    )
    if (inherits(con, "sockconn")) break
    if (Sys.time() - start > timeout) break
    Sys.sleep(poll)
  }

  # throw connection error if we never got a successful connection
  if (inherits(con, "error")) {
    stop(con)
  }

  # make a connection to the adapter, registering this session as the debugee
  write_message(con, request("initialize", list(clientName = "r-session")))
  write_message(con, request("attach", list(clientName = "r-session")))
  debuggee$new(con)
}
