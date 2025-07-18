.s7_object <- new_S3_class("S7_object")
.s7_class <- new_S3_class("S7_class")
.s7_union <- new_S3_class("S7_union")
.s7_base_class <- new_S3_class("S7_base_class")
.s7_s3_class <- new_S3_class("S7_S3_class")


#' Check whether a method is implemented for a given set of dispatch classes
#'
#' @keywords internal
#' @noRd
is_implemented <- function(...) {
  tryCatch(
    {
      S7::method(...)
      TRUE
    },
    error = function(e, ...) {
      FALSE
    }
  )
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
  function(subclass, class) identical(subclass, class)

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
  paste(c(attr(x, "package"), attr(x, "name")), collapse = "::")
}
