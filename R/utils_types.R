`%f||%` <- function(f1, f2) function(...) f1(...) || f2(...)
`%f&&%` <- function(f1, f2) function(...) f1(...) && f2(...)

is.scalar <- function(x) {
  length(x) == 1
}

is.string <- is.character %f&&% is.scalar
is.number <- is.numeric %f&&% is.scalar
is.boolean <- is.logical %f&&% is.scalar
is.any <- function(...) TRUE

is_dap_type <- function(type) function(x) inherits(x, type)
are_dap_type <- function(type) function(x) all(lapply(x, inherits, type))

validate_type_fields <- function(values, spec) {
  valnames <- names(values)
  if ((length(values) > 0 && is.null(valnames)) || any(valnames == "")) {
    stop("all fields must be named.")
  }

  req_fields <- grep("\\?$", names(spec), value = TRUE, invert = TRUE)
  if (any(i <- !req_fields %in% valnames)) {
    stop(paste0(
      "required fields missing: ",
      paste0(req_fields[[i]], collapse = ", ")
    ))
  }

  for (i in seq_along(spec)) {
    name <- gsub("\\?$", "", names(spec)[[i]])
    optional <- endsWith(names(spec)[[i]], "?")
    if (optional) next

    if (is.vector(spec[[i]])) {
      if (!isTRUE(values[[name]] %in% spec[[i]])) {
        stop(sprintf("field '%s' is not one of the accepted values", name))
      }
    } else if (is.function(spec)) {
      if (!isTRUE(spec[[i]](values[[name]]))) {
        stop(sprintf("field '%s' does not meet type definition", name))
      }
    }
  }

  values
}
