`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}



#' `vapply` shorthands
#'
#' Simple wrappers around `vapply` for common data types
#'
#' @rdname vapplys
#' @inheritParams base::vapply
#' @keywords internal
vlapply <- function(..., FUN.VALUE = logical(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

#' @rdname vapplys
vcapply <- function(..., FUN.VALUE = character(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

#' @rdname vapplys
vnapply <- function(..., FUN.VALUE = numeric(1L)) {
  vapply(..., FUN.VALUE = FUN.VALUE)
}

mfapply <- function(..., SIMPLIFY = FALSE) {
  mapply(..., SIMPLIFY = SIMPLIFY)
}


#' Package source file helpers
#'
#' Discover specific package related file paths
#'
#' @param path A path within a package source or install directory
#'
#' @name package-file-helpers
#' @keywords internal
find_package_root <- function(path = ".") {
  if (path == ".") path <- getwd()
  while (dirname(path) != path) {
    if (file.exists(file.path(path, "DESCRIPTION")))
      return(path)
    path <- dirname(path)
  }

  invisible(NULL)
}



find_package_name <- function(path = ".") {
  root <- find_package_root(path)
  if (is.null(root)) return(NULL)
  read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1]]
}


line_code_span <- function(path, line) {
  text <- scan(
    path,
    what = character(),
    n = 1,
    skip = line - 1,
    sep = "\n",
    quiet = TRUE
  )

  end <- nchar(text)
  start <- end - nchar(trimws(text, "left"))

  c(start, end)
}


is_success <- function(x) {
  isTRUE(attr(x, "success"))
}



is_valid_connection <- function(x) {
  as.numeric(x) %in% getAllConnections()
}



spec <- function(anchor = "") {
  specs_url <- "https://microsoft.github.io/debug-adapter-protocol/specification"
  sprintf("(\\url{%s%s})", specs_url, anchor)
}



regmatches <- function(pattern, text, ...) {
  m <- regexpr(pattern = pattern, text = text, ...)
  ml <- attr(m, "match.length")
  s <- attr(m, "capture.start")
  l <- attr(m, "capture.length")
  n <- attr(m, "capture.names")
  full <- substring(text, m, m + ml - 1)
  cap <- substring(text, s, s + l - 1)
  mat <- matrix(cap, ncol = length(n), dimnames = list(c(), n))
  colnames(mat) <- n
  rownames(mat) <- full
  mat
}
