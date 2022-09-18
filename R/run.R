#' @export
run <- function(..., debugger) {
  if (interactive()) {
    run_background_connection(...)
  } else {
    run_tcp_connection(..., debugger = debugger)
  }
}

run_stdio_connection <- function(..., debugger) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  con <- file("stdin", open = "rb", blocking = FALSE)
  adapter <- debug_adapter(con)

  if (!missing(debugger)) {
    adapter$debugger <- debugger
  }

  log(DEBUG, "Connection established")
  # TODO: listen on connection

  invisible(TRUE)
}


run_tcp_connection <- function(host = "localhost", port = 18721, poll = 100, debugger) {
  log(DEBUG, sprintf("Starting tcp server at %s:%s, awaiting DAP client ...", host, port))
  con <- socketConnection(host = host, port = port, server = TRUE, open = "r+b")
  adapter <- debug_adapter(con)

  if (!missing(debugger)) {
    adapter$debugger <- debugger
  }

  log(DEBUG, "Connection established")
  while (is_valid_connection(adapter$con)) {
    handle(adapter)
    Sys.sleep(poll / 1000)
  }

  invisible(TRUE)
}

run_background_connection <- function(...) {
  log(DEBUG, "Starting background stdio server, awaiting DAP client ...")
  bg <- callr::r_bg(
    function(...) {
      options(error = function(e) { print(traceback()); e })
      debugadapter:::run(...)
    },
    args = list(...)
  )

  adapter <- debug_adapter(bg$get_output_connection())
  addTaskCallback(name = "Background Debugger", function(...) {
    err <- bg$read_error()
    if (nchar(err) > 0) {
      echo(DEBUG, sprintf("[PID %.f] %s", bg$get_pid(), err))
    } else {
      status <- if (bg$is_alive()) "alive" else "stopped"
      log(DEBUG, sprintf("[PID %.f] %s", bg$get_pid(), status))
    }

    # handle any bg processes relayed back to parent session
    while (resp <- handle(adapter, timeout = 0.05)) {
      log(DEBUG, resp)
    }

    bg$is_alive()
  })

  invisible(TRUE)
}
