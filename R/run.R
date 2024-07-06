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
  log(DEBUG, "debugger connection: ", debugger)

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

run_background_connection <- function(port = 18721, ...) {
  log(DEBUG, "Starting background tcp server, awaiting DAP client ...")
  adapter_process <- start_adapter_in_background(port = port, ...)
  debuggee <- attach_runtime(port = port, timeout = 5)

  pid <- adapter_process$get_pid()
  options(browser.hook = browser_hook_sync_debugger(debuggee))

  addTaskCallback(name = "Synchronize Debugger", function(...) {
    status <- if (adapter_process$is_alive()) "alive" else "stopped"
    log(DEBUG, sprintf("background debugger on PID: %.f (%s)", pid, status))
    echo(DEBUG, adapter_process$read_error())

    if (!adapter_process$is_alive()) {
      close(debuggee$connection)
      tryCatch(
        adapter_process$get_result(),
        error = function(e) {
          e$message <- "in debug adapter running as background process"
          show(e)
          return()
        }
      )
    }

    # handle any bg processes relayed back to parent session
    while (!is.null(msg <- read_message(debuggee, timeout = 0.05))) {
      debuggee$handle(msg)
    }

    # keep callback as long as adapter is alive
    adapter_process$is_alive()
  })

  invisible(debuggee)
}
