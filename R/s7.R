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

class_trait_bound <- S7::new_class(
  name = "trait_bound",
  parent = class_function,
  properties = list(
    name = class_character    
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

convert <- S7::convert
S7::method(convert, list(class_function, class_trait_bound)) <- 
function(from, to, ...) {
  class_trait_bound(from)
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
    name = paste0("traitbound<", x$class, ">"),
    parent = x,
    validator = function(self) {
      print("Validating Scalar")
      if (length(self) != 1L) {
        return("must be length 1")
      }
    }
  )
}


my_type <- S7::new_class("my_type",
  properties = list(
    prop = NULL | (class_integer + scalar)    
  )
)
