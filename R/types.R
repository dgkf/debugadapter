public_fields <- function(x) {
  x[!startsWith(names(x), ".")]
}

class_trait_validator <- S7::new_class(
  "trait_validator",
  parent = class_function,
  constructor = function(value) {
    if (!is.list(value)) value <- list(value)
    default <- function(self) {
      output <- lapply(self@validators, do.call, list(self))
      Filter(Negate(is.null), output)
    }
    S7::new_object(default, validators = value)
  },
  properties = list(
    validators = class_list
  )
)

add_trait <- S7::new_generic("add_trait", dispatch_args = c("x", "t"))
add_trait_op <- function(e1, e2) { add_trait(e1, e2) }

"+.trait_validator" <- add_trait_op
S7::method(add_trait, list(class_trait_validator, class_trait_bound)) <- 
function(x, t) { 
  x@validators <- append(x@validators, t)
  x
}

"+.trait_validator" <- add_trait_op
S7::method(add_trait, list(class_function, class_trait_bound)) <- 
function(x, t) {
  class_trait_validator(x) + class_trait_bound(t)
}

"+.S7_base_class" <- add_trait_op
S7::method(add_trait, list(new_S3_class("S7_base_class"), class_function)) <-
function(x, t) {
  name <- match.call()[["t"]]
  add_trait(x, class_trait_bound(t, name = name))
}

S7::method(add_trait, list(new_S3_class("S7_base_class"), class_trait_bound)) <-
function(x, t) {
  new_class(
    name = paste0("bound_", x$class), 
    parent = x, 
    validator = add_trait(x$validator, t)
  )
}

class_trait_bound <- S7::new_class(
  name = "trait_bound",
  parent = class_function,
  properties = list(
    name = class_character    
  )
)

convert <- S7::convert
S7::method(convert, list(class_function, class_trait_bound)) <- 
function(from, to, ...) {
  class_trait_bound(from)
}

scalar <- function(value) {
  if (length(value) != 1L) {
    "must have length 1"
  }
}

new_class_validator <- function() {
  g <- S7::new_generic("validate", dispatch_args = "value")
  S7::method(g, NULL) <- function(value) { invisible() }
  g
}

list_of <- function(class, ...) {
  class_validator <- NULL
  class_name <- NULL
  is_correct_class <- NULL
  elem_validator <- NULL

  if (S7::S7_inherits(class)) {
    class_validator <- class@validator
    class_name <- class@name
    is_correct_class <- function(elem) inherits(elem, class)
  } else if (inherits(class, "S7_base_class") || inherits(class, "S7_S3_class")) {
    class_validator <- class$validator
    class_name <- class$class
    is_correct_class(function(elem) isa(elem, class_name))
  }

  if (is.null(class_validator)) {
    elem_validator <- function(value) { 
      if (!is_correct_class(value)) {
        paste0("elements must all be of class '", class_name, "'")
      }
    }
  } else {
    elem_validator <- function(value) {
      if (!is.null(resp <- class_validator(value))) {
        return(resp)
      } else if (!is_correct_class(value)) {
        paste0("elements must all be of class '", class_name, "'")
      }
    }
  }

  S7::new_class(
    name = paste0("list_of_", class_name),
    constructor = function(...) {
      S7::new_object(list(...))
    },
    validator = function(self) {
      for (i in seq_along(self)) {
        if (!is.null(resp <- elem_validator(self[[i]]))) return(resp)
      }
    }
  )
}

scalar <- S7::new_generic("scalar", dispatch_args = "x")

S7::method(scalar, S7::new_S3_class("S7_base_class")) <- function(x, ...) {
  S7::new_class(
    name = paste0("scalar<", x$class, ">"),
    parent = x,
    validator = function(self) {
      print("Validating Scalar")
      if (length(self) != 1L) {
        return("must be length 1")
      }
    }
  )
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

#' @import S7
class_breakpoint <- S7::new_class(
  "breakpoint",
  properties = list(
    id = S7::new_property(scalar(S7::class_integer) | NULL, default = NULL)
    verified = scalar(S7::class_logical),
    message = scalar(S7::class_character),
    source = class_source | NULL,
    line = scalar(S7::class_integer) | NULL,
    column  = scalar(S7::class_integer) | NULL,
    endLine = scalar(S7::class_integer) | NULL,
    endColumn = scalar(S7::class_integer) | NULL,
    instructionReference = scalar(S7::class_character) | NULL,
    offset = scalar(S7::class_integer) | NULL
  )
)

#' @describeIn dap_types
#' `r spec("#Types_Checksum")`
checksum <- dap_type("checksum",
  "algorithm" = c("MD5", "SHA1", "SHA256", "timestamp"),
  "checksum" = is.string
)

class_checksum <- new_class(
  "checksum",
  properties = list(
    algorithm = class_algorithm,
    checksum = scalar(class_character)
  )
)

class_algorithm <- S7::new_class(
  name = "algorithm",
  parent = S7::class_character,
  validator = function(self) {
    choices = c("MD5", "SHA1", "SHA256", "timestamp")
    if (!self %in% choices) {
      paste0("is not one of ", paste0("'", choices, "'", collapse = ", "))
    }
  }
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

class_presentation_hint <- new_class(
  name = "presentation_hint",
  parent = class_character,
  validator = function(self) {
    choices = c("normal", "emphasize", "deemphasize")
    if (!self %in% choices) {
      paste0("is not one of ", paste0("'", choices, "'", collapse = ", "))
    }
  }
)

source <- new_class("source",
  properties = list(
    name = NULL | (class_character + scalar)
    # path = NULL | scalar(class_character),
    # sourceReference = NULL | scalar(class_integer),
    # presentationHint = NULL | class_presentation_hint,
    # origin = NULL | scalar(class_character),
    # sources = list_of(new_S3_class("class_source")),
    # adapterData = class_any,
    # checksums = NULL | list_of(class_checksum)
  )
)
