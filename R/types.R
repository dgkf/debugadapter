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
dap_type <- function(.name, ...) {
  envir <- parent.frame()

  # load DAP type spec
  spec_exprs <- match.call(envir = envir)[-(1:2)]
  constructor <- function() {
    spec <- lapply(spec_exprs, eval, envir = envir)
    vals <- lapply(match.call()[-1], eval, envir = parent.frame())
    structure(
      validate_type_fields(vals, spec),
      class = c(.name, "dap_type", class(vals))
    )
  }

  # apply formal argument names
  frmls <- rep_len(as.list(formals(function(x) {})), length(spec_exprs))
  names(frmls) <- gsub("\\?$", "", names(spec_exprs))
  formals(constructor) <- frmls

  constructor
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

#' @describeIn dap_types
#' `r spec("#Types_Checksum")`
checksum <- dap_type("checksum",
  "algorithm" = c("MD5", "SHA1", "SHA256", "timestamp"),
  "checksum" = is.string
)

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
