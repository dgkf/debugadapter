#' Debug Adapter
#'
#' Starts a tcp server that listens for an arbitrary number of clients, and 
#' exactly one R environment in which to perform debugging operations. When
#' serving as debugger in attach mode, the R environment itself may also
#' operate as its own debug client.
#'
#' @param host The host to use for connections
#' @param port The port to use for connections
#' @param poll The polling frequency in seconds
#' @param timeout A timeout time in seconds to apply to all connections
#' @param ... Arguments used to initialize an [adapter]
#'
#' @name dap
NULL

#' @name dap
#' @export
run <- function(
  host = getOption("debugadapter.host", "localhost"),
  port = getOption("debugadapter.port", 18721),
  ...,
  wait_for_client = FALSE
) {
  if (interactive()) {
    DEBUG("Starting background tcp server, awaiting DAP client ...")
    px <- dap_launch_bg(host = host, port = port, ...)
    repl <- dap_attach(host = host, port = port, ..., process = px)
    if (wait_for_client) repl$await_client()
  } else {
    DEBUG("Starting tcp server, awaiting DAP client ...")
    dap_launch(port = port, ...)
  }
}

#' @name dap
#' @export
dap_attach <- function(
  host = getOption("debugadapter.host", "localhost"),
  port = getOption("debugadapter.port", 18721),
  ...,
  process = NULL
) {
  repl_client <- debuggee$new(host = host, port = port, ...)
  repl_prompt <- debug_prompt$new(repl_client)
  options(browser.hook = repl_prompt$browser_hook)
  insert_sync_callback(repl_client, process)
  repl_client
}

#' @name dap
#' @export
dap_launch <- function(...) {
  adapter$new(...)$run()
}

#' @name dap
dap_launch_bg <- function(..., host) {
  callr::r_bg(
    function(..., parent_options) {
      options(
        warn = 2L,
        debugadapter.log_prefix =
          paste0(cli::style_dim(cli::col_grey("(pid:", Sys.getpid(), ")"))),
        debugadapter.log = log
      )
      options(parent_options)
      debugadapter::dap_launch(...)
    },
    args = list(
      ...,
      parent_options = list(
        debugadapter.log = getOption("debugadapter.log"),
        cli.num_colors = cli::num_ansi_colors()
      )
    )
  )
}
