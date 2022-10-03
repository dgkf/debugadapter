#' @export
run <- function(...) {
  if (interactive()) {
    run_background_connection(...)
  } else {
    run_tcp_connection(...)
  }
}

run_stdio_connection <- function(..., poll = 100, debugger) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  con <- file("stdin", open = "rb", blocking = FALSE)
  adapter <- debug_adapter(con)

  if (!missing(debugger)) {
    adapter$debugger <- debugger
  }

  log(DEBUG, "Connection established")
  while (isOpen(con)) {
    handle(adapter)
    Sys.sleep(poll / 1000)
  }

  invisible(TRUE)
}


run_tcp_connection <- function(host = "localhost", port = 18721, poll = 100, debugger = stdout()) {
  log(DEBUG, sprintf("Starting tcp server at %s:%s, awaiting DAP client ...", host, port))
  con <- socketConnection(host = host, port = port, server = TRUE, open = "r+b")
  adapter <- debug_adapter(con)

  log(DEBUG, "Connection established")
  while (is_valid_connection(adapter$con)) {
    # echo responses back to debugger
    if (is_response(res <- handle(adapter))) {
      write_message(debugger, res)
    }

    Sys.sleep(poll / 1000)
  }

  invisible(TRUE)
}

run_background_connection <- function(...) {
  log(DEBUG, "Starting background tcp server, awaiting DAP client ...")

  bg <- callr::r_bg(
    function(...) {
      options(debugadapter.log_prefix = "[BG] ")
      options(error = function(e) { print(traceback()); e })
      debugadapter:::run(...)
    },
    args = list(...)
  )

  debugger <- debug_in_foreground(bg$get_output_connection())

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
    while (debugger_handle(debugger, timeout = 0.05)) { }

    bg$is_alive()
  })

  invisible(TRUE)
}
