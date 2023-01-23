public_fields <- function(x) {
  x[!startsWith(names(x), ".")]
}



#' DAP types constructors
#'
#' Internal DAP type constructors are built with a mini type syntax, which
#' performs some light data validation and builds type constructors. Data is
#' returned as a simple S3 object, which can be easily packaged as a JSON
#' message body. The goal of this mini type language is to keep the syntax of
#' type specifications as close to their language-agnostic JSON-schema type
#' specification as possible.
#'
#' @param .name The name of DAP type (in snake-case, instead of camel-case)
#' @param ... Additional arguments should be named similarly to the DAP
#'   specification, with a trailing `?` if the value is optional. The param
#'   value should be a predicate function used for field data validation.
#'
#' @name dap_types
#'
dap_type <- function(.name, ..., .attrs = list()) {
  envir <- parent.frame()

  # load DAP type spec
  call_args <- as.list(match.call(envir = envir)[-1])
  spec_exprs <- call_args[!names(call_args) %in% names(formals())]

  constructor <- function() {
    spec <- lapply(spec_exprs, eval, envir = envir)
    vals <- lapply(sys.call()[-1], eval, envir = parent.frame())

    obj <- structure(
      validate_type_fields(vals, spec),
      class = c(.name, "dap_type", class(vals))
    )

    attributes(obj)[names(.attrs)] <- .attrs
    obj
  }

  # apply formal argument names
  frmls <- rep_len(as.list(formals(function(x) NULL)), length(spec_exprs))
  names(frmls) <- gsub("\\?$", "", names(spec_exprs))
  formals(constructor) <- frmls

  constructor
}

dap_event <- function(.name, ...) {
  dap_type(.name, ..., .attrs = list(event = .name))
}

#' Coerce fields in a list to a DAP type struct
#'
#' @name dap_types
#'
to_dap_type <- function(type, fields, ...) {
  do.call(type, append(fields, list(...)))
}



#' @describeIn dap_types
#' `r spec("#Types_Breakpoint")`
breakpoint <- dap_type("breakpoint",
  "id?" = is.number,
  "verified" = is.boolean,
  "message?" = is.string,
  "source?" = is_dap_type("source"),
  "line?" = is.number,
  "column?" = is.number,
  "endLine?" = is.number,
  "endColumn?" = is.number,
  "instructionReference?" = is.string,
  "offset?" = is.number
)

#' @export
format.breakpoint <- function(x, ...) {
  paste0(
    sprintf("%.f: ", x$id),
    format(x$source), " ",
    "[", x$line, ":", x$column, ":", x$endLine, ":", x$endColumn, "]"
  )
}

as.breakpoint <- function(x, ...) {
  x$source <- to_dap_type(source, x$source, ...)
  to_dap_type(breakpoint, x, ...)
}



#' @describeIn dap_types
#' `r spec("#Types_Checksum")`
checksum <- dap_type("checksum",
  "algorithm" = c("MD5", "SHA1", "SHA256", "timestamp"),
  "checksum" = is.string
)

#' @export
format.checksum <- function(x, ..., short = TRUE) {
  hash <- if (isTRUE(short)) substring(x$checksum, 1, 8) else x$checksum
  paste0(hash, "[", x$algorithm, "]")
}



#' @describeIn dap_types
#' `r spec("#Types_Source")`
source <- dap_type("source",
  "name?" = is.string,
  "path?" = is.string,
  "sourceReference?" = is.number,
  "presentationHint?" = c("normal", "emphasize", "deemphasize"),
  "origin?" = is.string,
  "sources?" = are_dap_type("source"),
  "adapterData?" = is.any,
  "checksums?" = are_dap_type("checksum")
)

#' @export
format.source <- function(x, ...) {
  x$path
}



#' @describeIn dap_types
#' `r spec("#Events_Stopped")`
stopped <- dap_event("stopped",
  "reason" = is.string %f||% is_one_of(
    "step",
    "breakpoint",
    "exception",
    "pause",
    "entry",
    "goto",
    "function breakpoint",
    "data breakpoint",
    "instruction breakpoint"
  ),
  "description?" = is.string,
  "threadId?" = is.number,
  "preserveFocusHint?" = is.boolean,
  "text?" = is.string,
  "allThreadsStopped?" = is.boolean,
  "hitBreakpointIds?" = is_list_of(is.numeric)
)
