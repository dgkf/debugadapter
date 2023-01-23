`%f||%` <- function(f1, f2) function(...) f1(...) || f2(...)
`%f&&%` <- function(f1, f2) function(...) f1(...) && f2(...)

is.scalar <- function(x) { length(x) == 1 }
is.any <- function(...) TRUE

is_one_of <- function(x) { function(y) x %in% y }
is_list_of <- function(x) {
  structure(
    function(y) all(vlapply(y, x)),
    transform = as.list  # allow vectors, coerced to list
  )
}

is.string <- is.character %f&&% is.scalar
is.number <- is.numeric %f&&% is.scalar
is.boolean <- is.logical %f&&% is.scalar

is_dap_type <- function(type) function(x) inherits(x, type)
are_dap_type <- function(type) function(x) all(lapply(x, inherits, type))

validate_type_fields <- function(values, spec) {
  valnames <- names(values)
  if ((length(values) > 0 && is.null(valnames)) || any(valnames == "")) {
    stop("all fields must be named.")
  }

  req_fields <- grep("?$", names(spec), value = TRUE, invert = TRUE)
  if (any(i <- !req_fields %in% valnames)) {
    stop(paste0(
      "required fields missing: ",
      paste0(req_fields[[i]], collapse = ", ")
    ))
  }

  for (i in seq_along(spec)) {
    name <- gsub("\\?$", "", names(spec)[[i]])
    optional <- endsWith(names(spec)[[i]], "?")
    if (optional && !name %in% names(values)) next

    # if an associated transform is provided, apply before validation
    val <- values[[name]]
    if (is.function(transform <- attr(spec[[i]], "transform"))) {
      val <- transform(val)
    }

    if (is.vector(spec[[i]])) {
      if (!isTRUE(val %in% spec[[i]])) {
        stop(sprintf("field '%s' is not one of the accepted values", name))
      }
    } else if (is.function(spec)) {
      if (!isTRUE(spec[[i]](val))) {
        stop(sprintf("field '%s' does not meet type definition", name))
      }
    }

    values[[name]] <- val
  }

  values
}
