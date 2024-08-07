% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adapter.R
\name{adapter}
\alias{adapter}
\title{Adapter}
\description{
The debug adapter server, which manages connections to an arbitrary number
of clients which may interact with the debug state. The adapter expects one
client to be a priveleged "debuggee", which will provide information about
the debug state. This will be an active R session, which is expected to
have access to the symbols requested for debugging.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{server}}{A socket server, listening for connections on 'port'}

\item{\code{port}}{A port on which to listen for new client connections}

\item{\code{poll}}{The polling frequency with which the port should be
scanned for new connections or messages}

\item{\code{timeout}}{A timeout to use for new connections}

\item{\code{clients}}{A optionally-named list of clients. If one is given
the name 'debuggee', it is used as the R session which will
be used for managing debug state. As an attached R session can serve
as both a client (used to execute debugging actions) and a debuggee
(used as the basis for debugging state), the distinction between client
and debuggee is left somewhat ambiguous in this implementation.}

\item{\code{breakpoints}}{A list of breakpoints}

\item{\code{breakpoint_id}}{An incremented unique identifier for each
breakpoint}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{debuggee}}{Retrieve debuggee client}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-adapter-new}{\code{adapter$new()}}
\item \href{#method-adapter-run}{\code{adapter$run()}}
\item \href{#method-adapter-process_requests}{\code{adapter$process_requests()}}
\item \href{#method-adapter-relay_to_clients}{\code{adapter$relay_to_clients()}}
\item \href{#method-adapter-open_new_connections}{\code{adapter$open_new_connections()}}
\item \href{#method-adapter-close_timedout_connections}{\code{adapter$close_timedout_connections()}}
\item \href{#method-adapter-set_debuggee}{\code{adapter$set_debuggee()}}
\item \href{#method-adapter-next_breakpoint_id}{\code{adapter$next_breakpoint_id()}}
\item \href{#method-adapter-set_pending_breakpoints}{\code{adapter$set_pending_breakpoints()}}
\item \href{#method-adapter-set_breakpoints}{\code{adapter$set_breakpoints()}}
\item \href{#method-adapter-close_connections}{\code{adapter$close_connections()}}
\item \href{#method-adapter-clone}{\code{adapter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-new"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$new(port = 18721, poll = 0.1, timeout = 60, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{port}}{\code{integer} The port on which to host a tcp server}

\item{\code{poll}}{\code{numeric} The frequency at which connections should be
polled for new messages}

\item{\code{timeout}}{\code{numeric} An inactivity time in seconds, after which
connections should be closed.}

\item{\code{...}}{Additional arguments unused}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
self
The core loop of the adapter, handling connections and message passing
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-run"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-run}{}}}
\subsection{Method \code{run()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$run()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
will not return unless an error is encountered
Iterate through connections and handle requests
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-process_requests"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-process_requests}{}}}
\subsection{Method \code{process_requests()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$process_requests()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
used for side effect of listening to and handling messages from
clients.
Propagate a message back out to all attached clients
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-relay_to_clients"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-relay_to_clients}{}}}
\subsection{Method \code{relay_to_clients()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$relay_to_clients(content, debuggee = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{content}}{\code{list} The message contents to relay}

\item{\code{debuggee}}{\code{logical} indicating whether the debuggee client should
be included.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Used for side effects of message passing

Scan port for attempts to form new connections
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-open_new_connections"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-open_new_connections}{}}}
\subsection{Method \code{open_new_connections()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$open_new_connections()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
used for side effect of opening new connections
Close connections that have outlived their timeout
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-close_timedout_connections"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-close_timedout_connections}{}}}
\subsection{Method \code{close_timedout_connections()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$close_timedout_connections()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
used for side effect of closing stale connections
Mark a client as a debuggee
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-set_debuggee"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-set_debuggee}{}}}
\subsection{Method \code{set_debuggee()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$set_debuggee(client)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{client}}{A \link{client} to tag as the debuggee}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Used for side effect of mutating names of clients list

Increment and return new breakpoint ids
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-next_breakpoint_id"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-next_breakpoint_id}{}}}
\subsection{Method \code{next_breakpoint_id()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$next_breakpoint_id(n = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{A number of new identifiers to create}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{integer[n]} A sequence of identifier integers of length \code{n}

Create breakpoints, store and reply in pending state
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-set_pending_breakpoints"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-set_pending_breakpoints}{}}}
\subsection{Method \code{set_pending_breakpoints()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$set_pending_breakpoints(content)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{content}}{the \code{setBreakpoints} message content

Set breakpoints, most often after first creating them as 'pending' and
then passing them off to the debuggee for verification.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-set_breakpoints"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-set_breakpoints}{}}}
\subsection{Method \code{set_breakpoints()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$set_breakpoints(content)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{content}}{the \code{setBreakpoints} out-of-spec response content from
the debuggee, which is expected to be a viable response to the
original \code{setBreakpoint} request that can be immediately relayed
to clients.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
used for side-effects of updating state and relaying message
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-close_connections"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-close_connections}{}}}
\subsection{Method \code{close_connections()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$close_connections()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-adapter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-adapter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{adapter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
