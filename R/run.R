#' @export
run <- function(..., debugger) {
  if (missing(debugger) && interactive()) {
    run_background_stdio_connection(...)
  } else {
    run_stdio_connection(..., debugger = debugger)
  }
}

run_stdio_connection <- function(host = "localhost", port = 18721, poll = 100, debugger) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  adapter <- debug_adapter(socketConnection(
    host = host,
    port = port,
    server = TRUE,
    open = "r+b"
  ))

  if (!missing(debugger))
    adapter$debugger <- debugger

  log(DEBUG, "Server connected")
  repeat {
    handle(adapter)
    Sys.sleep(poll / 1000)
  }

  close(adapter)
}

run_background_stdio_connection <- function(...) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  adapter <- debug_adapter(callr::r_bg(
    function(...) dapr:::run(..., debugger = dapr::debug_stdout_relay()),
    args = list(...)
  ))

  addTaskCallback(name = "Background Debugger", function(...) {
    echo(DEBUG, adapter$con$read_error())

    # handle any bg processes relayed back to parent session
    repeat {
      msg <- read_message(adapter$con$get_output_connection())
      if (is.null(msg)) break
      debugger_handle(debug_here(), msg)
    }

    # TODO: remove callback when process has finished
    TRUE
  })

  invisible(TRUE)
}
