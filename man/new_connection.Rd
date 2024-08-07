% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server.R
\name{new_connection}
\alias{new_connection}
\title{Attach the Current Session to a Debug Adapter}
\usage{
new_connection(host, port, poll = 0.1, timeout = 0)
}
\value{
A socket connection to a debug adapter
}
\description{
Connects the current session as the 'attached' session to a debug, adapter.
The current session will be used for managing the debugging environment.
}
