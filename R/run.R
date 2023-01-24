#' @export
run <- function(...) {
  if (interactive()) {
    run_background_connection(...)
  } else {
    run_tcp_connection(...)
  }
}

run_stdio_connection <- function(..., poll = 100) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  con <- file("stdin", open = "rb", blocking = FALSE)
  adapter <- debug_adapter(con)

  log(DEBUG, "Connection established")
  while (isOpen(con)) {
    handle(adapter)
    Sys.sleep(poll / 1000)
  }

  invisible(TRUE)
}

run_tcp_connection <- function(
  host = "127.0.0.1",
  port = 18721,
  poll = 100,
  debugger = NULL
) {

  msg <- sprintf("Starting tcp server at %s:%s, awaiting DAP client ...", host, port)
  log(DEBUG, msg)

  con <- socketConnection(host = host, port = port, server = TRUE, open = "r+b")
  log(DEBUG, "Connection established")

  adapter <- debug_adapter(con, debugger = debugger)
  while (is_valid_connection(adapter$con)) {
    # process latest messages from adapter client, echoing output to debugger
    echo_responses(handle(adapter), to = debugger)

    # flush debugger output back to adapter
    flush_messages(from = debugger, to = adapter, timeout = 0.01)

    Sys.sleep(poll / 1000)
  }

  invisible(TRUE)
}

run_background_connection <- function(...) {
  log(DEBUG, "Starting background tcp server, awaiting DAP client ...")

  # adapter is hosted in the background and handles protocol
  bg <- callr::r_bg(
    function(...) {
      options(debugadapter.log_prefix = "[BG SRV]")
      debugger_client <- socketConnection(
        host = "localhost",
        port = 18722,
        server = FALSE
      )

      debugadapter:::run(..., debugger = debugger_client)
    },
    args = list(...),
  )

  # debugger is hosted in the foreground and handles debug state
  debugger <- debug_in_foreground(socketConnection(
    host = "localhost",
    port = 18722,
    server = TRUE,
    open = "r+b"
  ))

  pid <- bg$get_pid()
  addTaskCallback(name = "Background Debugger", function(...) {
    err <- bg$read_error()
    if (nchar(err) > 0) {
      log(DEBUG, sprintf("background debugger started on PID: %.f", pid))
      echo(DEBUG, err)
    } else {
      status <- if (bg$is_alive()) "alive" else "stopped"
      log(DEBUG, sprintf("background debugger on PID: %.f (%s)", pid, status))
    }

    # handle any bg processes relayed back to parent session
    while (debugger_handle(debugger, timeout = 0.05)) NULL

    bg$is_alive()
  })

  invisible(TRUE)
}
