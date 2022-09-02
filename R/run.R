#' @export
run <- function(..., debugger) {
  if (interactive()) {
    run_background_stdio_connection(...)
  } else {
    run_stdio_connection(..., debugger = debugger)
  }
}

run_stdio_connection <- function(host = "localhost", port = 18721, poll = 100, debugger) {
  log(DEBUG, "Starting stdio server, awaiting DAP client ...")
  con <- socketConnection(
    host = host,
    port = port,
    server = TRUE,
    open = "r+b"
  )

  adapter <- debug_adapter(con)
  if (!missing(debugger))
    adapter$debugger <- debugger

  while (is_valid_connection(adapter$con)) {
    handle(adapter)
    Sys.sleep(poll / 1000)
  }
}

run_background_stdio_connection <- function(...) {
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
      echo(DEBUG, paste0("[PID ", bg$get_pid(), "] ", err))
    }

    # handle any bg processes relayed back to parent session
    while (resp <- handle(adapter, timeout = 0.05)) {
      log(DEBUG, resp)
    }

    bg$is_alive()
  })

  invisible(TRUE)
}
