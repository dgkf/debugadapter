#' Debug Adapter
#'
#' Starts a tcp server that listens for an arbitrary number of clients, and 
#' exactly one R environment in which to perform debugging operations. When
#' serving as debugger in attach mode, the R environment itself may also
#' operate as its own debug client. 
#'
#' @param port The port to listen on for new connections
#' @param poll The polling frequency in seconds
#' @param timeout A timeout time in seconds to apply to all connections
#' @param ... Arguments used to initialize an [adapter]
#'
#' @name start_adapter
#' @export
start_adapter <- function(...) {
  adapter$new(...)$run()
}

#' @name start_adapter
start_adapter_in_background <- function(...) {
  callr::r_bg(
    function(..., parent_options) {
      options(
        warn = 2L,
        debugadapter.log_prefix =
          paste0(cli::style_dim(cli::col_grey("(pid:", Sys.getpid(), ")"))),
        debugadapter.log = log
      )
      options(parent_options)
      debugadapter::start_adapter(...)
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

#' Adapter
#'
#' The debug adapter server, which manages connections to an arbitrary number
#' of clients which may interact with the debug state. The adapter expects one
#' client to be a priveleged "debuggee", which will provide information about
#' the debug state. This will be an active R session, which is expected to
#' have access to the symbols requested for debugging.
#'
adapter <- R6::R6Class(  # nolint
  "adapter",
  public = list(
    #' @field server A socket server, listening for connections on 'port'
    server = NULL,

    #' @field port A port on which to listen for new client connections
    port = 18721,

    #' @field poll The polling frequency with which the port should be
    #' scanned for new connections or messages
    poll = 0.1,

    #' @field timeout A timeout to use for new connections
    timeout = 60,

    #' @field clients A optionally-named list of clients. If one is given
    #' the name 'debuggee', it is used as the R session which will
    #' be used for managing debug state. As an attached R session can serve
    #' as both a client (used to execute debugging actions) and a debuggee
    #' (used as the basis for debugging state), the distinction between client
    #' and debuggee is left somewhat ambiguous in this implementation.
    clients = list(),

    #' @field breakpoints A list of breakpoints
    breakpoints = list(),

    #' @field breakpoint_id An incremented unique identifier for each
    #' breakpoint
    breakpoint_id = 0,

    #' @param port `integer` The port on which to host a tcp server
    #' @param poll `numeric` The frequency at which connections should be
    #'   polled for new messages
    #' @param timeout `numeric` An inactivity time in seconds, after which
    #'   connections should be closed.
    #' @param ... Additional arguments unused
    #' @return self
    initialize = function(port = 18721, poll = 0.1, timeout = 60, ...) {
      DEBUG(
        "Starting tcp server at localhost:", port,
        ", listening for DAP client ..."
      )

      self$timeout <- timeout
      self$port <- port
      self$server <- serverSocket(port = port)
      self
    },

    #' The core loop of the adapter, handling connections and message passing
    #' @return will not return unless an error is encountered
    run = function() {
      repeat {
        self$open_new_connections()
        self$process_requests()
        self$close_timedout_connections()
        Sys.sleep(self$poll)
      }
    },

    #' Iterate through connections and handle requests
    #' @return used for side effect of listening to and handling messages from
    #'   clients.
    process_requests = function() {
      for (client in self$clients) {
        while (!is.null(msg <- read_message(client, timeout = Inf))) {
          handle(msg, adapter = self, client = client)
        }
      }
    },

    #' Propagate a message back out to all attached clients
    #'
    #' @param content `list` The message contents to relay
    #' @param debuggee `logical` indicating whether the debuggee client should
    #'   be included.
    #' @return Used for side effects of message passing
    #'
    relay_to_clients = function(content, debuggee = FALSE) {
      clients <- self$clients
      if (!debuggee && !is.null(names(clients))) {
        clients <- clients[names(clients) != "debuggee"]
      }
      for (client in clients) write_message(client, content)
    },

    #' Scan port for attempts to form new connections
    #' @return used for side effect of opening new connections
    open_new_connections = function() {
      while (is_conn_waiting <- socketSelect(list(self$server), timeout = 0)) {
        # catch errors, in exceedingly rare case where connection stopped
        # requesting between expressions
        tryCatch({
          conn <- suppressWarnings(socketAccept(self$server, timeout = 1))
          socketTimeout(conn, self$timeout)
          n <- length(self$clients) + 1L
          DEBUG("new connection (", n, ") accepted on port ", self$port)
          self$clients[[n]] <- client$new(conn)
        }, error = function(e) {
          message(conditionMessage(e))
        })
      }
    },

    #' Close connections that have outlived their timeout
    #' @return used for side effect of closing stale connections
    close_timedout_connections = function() {
      t <- Sys.time()
      to_close <- vlapply(self$clients, function(client) {
        Sys.time() - client$time_last_message > self$timeout
      })

      if (!any(to_close)) {
        return()
      }

      for (i in which(to_close)) {
        close(self$client[[i]]$connection)
        DEBUG("connection ", i, " closed on port ", self$port)
      }
    },

    #' Mark a client as a debuggee
    #'
    #' @param client A [client] to tag as the debuggee
    #' @return Used for side effect of mutating names of clients list
    #'
    set_debuggee = function(client) {
      if (!is.null(self$debuggee)) {
        DEBUG("debuggee requested, but a debuggee is already attached")
      }
      client_ids <- vnapply(self$clients, function(client) client$id())
      i <- which(client$id() == client_ids)
      names(self$clients)[i] <- "debuggee"
    },

    #' Increment and return new breakpoint ids
    #'
    #' @param n A number of new identifiers to create
    #' @return `integer[n]` A sequence of identifier integers of length `n`
    #'
    next_breakpoint_id = function(n = 1) {
      self$breakpoint_id <- self$breakpoint_id + n
      self$breakpoint_id - (n - seq_len(n))
    },

    #' Create breakpoints, store and reply in pending state
    #'
    #' @param content the `setBreakpoints` message content
    #'
    set_pending_breakpoints = function(content) {
      # Convert message arguments to pending breakpoints
      args <- content$arguments
      size_hint_obj <- args$breakpoints %||% args$lines
      ids <- self$next_breakpoint_id(length(size_hint_obj))
      breakpoints <- breakpoints_parse_as_pending(args, ids)

      # update local index of breakpoints
      self$breakpoints[as.character(ids)] <- breakpoints
      content$arguments$breakpoints <- breakpoints

      # send to debuggee for verification
      if (!is.null(self$debuggee)) {
        write_message(self$debuggee, content)
      }
    },

    #' Set breakpoints, most often after first creating them as 'pending' and
    #' then passing them off to the debuggee for verification.
    #'
    #' @param content the `setBreakpoints` out-of-spec response content from
    #'   the debuggee, which is expected to be a viable response to the
    #'   original `setBreakpoint` request that can be immediately relayed
    #'   to clients.
    #' @return used for side-effects of updating state and relaying message
    #'
    set_breakpoints = function(content) {
      # update our local adapter breakpoints to reflect breakpoints in debuggee
      ids <- vnapply(content$breakpoints, `[[`, "id")
      self$breakpoints[as.character(ids)] <- content$breakpoints

      # relay response back out to clients
      self$relay_to_clients(content)
    }
  ),
  active = list(
    #' @field debuggee Retrieve debuggee client
    debuggee = function() self$clients[["debuggee"]]
  )
)


#' Debug Adapter
#'
#' A simple class which stores state for a debug adapter. Operationally, this is
#' an environment for in-place mutation of the adapter state.
#'
#' @param x A connection object used to communicate with the debug client
#' @keywords internal
debug_adapter <- function(x, ...) {
  structure(list(), state = debug_state(x, ...), class = "debug_adapter")
}

#' @param con An adapter base R connection or processx connection object
#' @param breakpoints A list of breakpoints currently in place
#' @param client A list of capabilities of the client
#' @param debugger A debugger object, used for dispatching behavior based on
#'   debugger type.
#'
#' @describeIn debug_adapter
#' A simple S3 wrapper around an environment used to track debug adapter state.
#'
#' @keywords internal
debug_state <- function(con = NULL, breakpoints = list(), client = list(),
  capabilities = debug_adapter_capabilities(), debugger = NULL) {
  structure(
    as.environment(list(
      con = con,
      breakpoints = breakpoints,
      client = client,
      capabilities = capabilities,
      debugger = debugger,
      breakpoint_id = 0
    )),
    class = "debug_state"
  )
}

#' @describeIn debug_adapter
#' Accessors for the debug adapter
`$.debug_adapter` <- function(x, name) {
  attr(x, "state")[[as.character(name)]]
}

#' @describeIn debug_adapter
#' Assignment to stateful components of the debug adapter
`$<-.debug_adapter` <- function(x, name, value) {
  state <- attr(x, "state")
  state[[as.character(name)]] <- value
  x
}

#' @describeIn debug_adapter
#' Generate a unique id number for breakpoints
#' @keywords internal
next_id <- function(adapter, n = 1) {
  seq(
    from = adapter$breakpoint_id + 1,
    to = adapter$breakpoint_id <- adapter$breakpoint_id + n
  )
}

#' @describeIn debug_adapter
#' Test whether adapter supports a capability
#' @keywords internal
supports <- function(x, capability) {
  n <- utils::head(grep(
    paste0("supports?", capability),
    names(x$capabilities),
    value = TRUE,
    ignore.case = TRUE
  ), 1)

  if (length(n) < 1 || !n %in% names(x$capabilities)) {
    DEBUG(sprintf("Cannot find capability '%s'", capability))
    return(FALSE)
  }

  isTRUE(x$capabilities[[n[[1]]]])
}

#' @describeIn debug_adapter
#' Convert a debug adapter to a string representation
#' @keywords internal
format.debug_adapter <- function(x) {
  s <- attr(x, "state")
  class(x) <- setdiff(class(x), "debug_adapter")
  paste0(
    "<debug adapter>\n",
    paste0(capture.output(x), collapse = "\n"),
    paste0(
      vcapply(names(s), function(n) paste0(n, ":\n", capture.output(s[[n]]))),
      collapse = "\n"
    )
  )
}
