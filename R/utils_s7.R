.s7_object <- new_S3_class("S7_object")
.s7_class <- new_S3_class("S7_class")
.s7_union <- new_S3_class("S7_union")
.s7_base_class <- new_S3_class("S7_base_class")
.s7_s3_class <- new_S3_class("S7_S3_class")


#' Check whether a method is implemented for a given set of dispatch classes
#'
#' @keywords internal
#' @noRd
is_implemented <- function(generic, class, ...) {
  tryCatch(
    {
      m <- S7::method(generic, class, ...)
      identical(as_signature_vector(class), as_signature_vector(m@signature))
    },
    error = function(e, ...) {
      FALSE
    }
  )
}

#' Create a character representation of classes
#' 
#' @note Internally, `S7` uses character representations to handle dispatch.
#'   When comparing the signatures of methods, classes themselves will not be
#'   identical because they carry artifacts of their creation environment.
#'   We can disregard these differences by comparing only the string
#'   representation.
#'   
#' @keywords internal
#' @noRd 
as_signature_vector <- function(signature) {
  # S7 classes are lists, we want a list of S7 classes
  if (!identical(class(signature), "list")) {
    signature <- list(signature)
  }
  
  vcapply(signature, class_desc)
}

#' Suppress `S7` messages when overwriting an existing method implementation
#' 
#' @keywords internal
#' @noRd
suppress_method_overwrite <- function(expr) {
  withCallingHandlers(expr, message = function(m) {
    if (startsWith(tolower(m$message), "overwriting")) {
      invokeRestart("muffleMessage")
    }
  })
}

#' Check whether an object inherits from a class
#'
#' @keywords internal
#' @noRd
is <- new_generic("is", c("x", "class"))

method(is, list(class_any, class_any)) <-
  function(x, class) is(x, S7::as_class(class))

method(is, list(class_any, .s7_base_class)) <-
  function(x, class) inherits(x, class$class)

method(is, list(class_any, .s7_s3_class)) <-
  function(x, class) inherits(x, class$class)

method(is, list(.s7_object, .s7_class)) <-
  function(x, class) S7::S7_inherits(x = x, class = class)

method(is, list(class_any, .s7_union)) <-
  function(x, class) {
    if (identical(class, class_numeric)) {
      is.numeric(x)
    } else {
      any(vlapply(class$classes, x = x, is))
    }
  }

#' Check whether a class inherits from another class
#'
#' @keywords internal
#' @noRd
is_subclass <- new_generic("is_subclass", c("subclass", "class"))

method(is_subclass, list(class_any, class_any)) <-
  function(subclass, class) FALSE

method(is_subclass, list(.s7_base_class, .s7_base_class)) <-
  function(subclass, class) {
    # only compare class name, ignore constructor which binds environment
    identical(subclass$class, class$class)
  }

method(is_subclass, list(.s7_class, .s7_class)) <-
  function(subclass, class) {
    if (identical(subclass, class)) {
      return(TRUE)
    }
    is_subclass(subclass@parent, class)
  }

method(is_subclass, list(class_any, .s7_union)) <-
  function(subclass, class) {
    any(vlapply(class$classes, subclass = subclass, is_subclass))
  }

#' lifted from S7:::class_desc
#'
#' @keywords internal
#' @noRd
class_desc <- function(x) {
  getNamespace("S7")$class_desc(x)
}
