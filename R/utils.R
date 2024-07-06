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
  if (is.null(path)) return(NULL)
  if (path == ".") path <- getwd()
  while (dirname(path) != path) {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    path <- dirname(path)
  }

  invisible(NULL)
}


find_package_name <- function(path = ".") {
  root <- find_package_root(path)
  if (is.null(root)) {
    return(NULL)
  }
  read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1]]
}


#' Find Source of Object
#'
#' When debugging, many objects are modified with injected traces. We don't
#' care about these because they only exist in our R Session. What we want
#' is the original object whose source may be opened in a client editor.
#'
#' @param expr In most uses, an expression. Dispatch is used to recursively
#'   discover source objects after evaluation, so after recursing, this might
#'   be an R object.
#' @param envir An environment in which to evaluate the expression
#'
find_source_object <- function(expr, envir) {
  UseMethod("find_source_object")  
}

#' @export
find_source_object.default <- function(expr, envir) {
  expr
}

#' @export
find_source_object.quote <- function(expr, envir) {
  find_source_object(tryCatch(
    eval(expr, envir = frame),
    error = function(e) NULL
  ))
}

#' @export
find_source_object.functionWithTrace <- function(expr, envir) {
  find_source_object(attr(expr, "original"))
}

#' @export
find_source_object.name <- find_source_object.quote

#' @export
find_source_object.expression <- find_source_object.quote


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


dispatch_on <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  structure(TRUE, class = x)
}


strip_empty_lines <- function(x) {
  gsub("(\n|\r|\n\r|\r\n){2,}", "\\1", x)
}


#' @export
srcpos <- function() {
  .Call("srcpos")
}


#' Format a variable for return as part of a Variable.value type
#'
#' @param x Any R object
#'
format_variable <- function(x) {
  UseMethod("format_variable")  
}

#' @export
format_variable.default <- function(x) {
  out <- deparse(x, nlines = 1, width.cutoff = 40)
  if (nchar(out) == 40) paste0(out, "...") else out
}

#' @export
format_variable.data.frame <- function(x) {
  paste0("<data.frame ", nrow(x), "x", ncol(x), ">")
}

#' @export
format_variable.array <- function(x) {
  d <- dim(x)  
  type <- if (length(d) <= 2) "matrix" else "array"
  paste0("<", type, " ", paste0(d, collapse = "x"), ">")
}
