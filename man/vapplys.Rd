% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{vlapply}
\alias{vlapply}
\alias{vcapply}
\alias{vnapply}
\title{\code{vapply} shorthands}
\usage{
vlapply(..., FUN.VALUE = logical(1L))

vcapply(..., FUN.VALUE = character(1L))

vnapply(..., FUN.VALUE = numeric(1L))
}
\arguments{
\item{...}{optional arguments to \code{FUN}.}

\item{FUN.VALUE}{a (generalized) vector; a template for the return
    value from FUN.  See \sQuote{Details}.}
}
\description{
Simple wrappers around \code{vapply} for common data types
}
\keyword{internal}
