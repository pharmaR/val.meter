#' @export
encode_dcf <- S7::new_generic("encode_dcf", "x")

#' @export
decode_dcf <- S7::new_generic("decode_dcf", c("x", "to"))

#' @export
method(encode_dcf, class_any) <- function(x, ...) {
  paste(deparse(x), collapse = " ")
}

#' @export
method(encode_dcf, class_atomic) <- function(x, ...) {
  x <- unclass(x)
  attributes(x) <- attributes(x)[intersect(names(attributes(x)), "names")]
  paste(deparse(x), collapse = " ")
}

#' @include utils_s7.R
#' @export
method(decode_dcf, list(class_any, .s7_class)) <- function(x, to, ...) {
  convert(structure(x, class = c("dcf", class(x))), to)
}

#' @export
method(decode_dcf, list(class_any, class_any)) <- function(x, to, ...) {
  eval(parse(text = x), envir = topenv())
}

#' @export
method(decode_dcf, list(class_any, class_missing)) <- function(x, to, ...) {
  decode_dcf(x, class_pkg)
}
