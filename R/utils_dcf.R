#' @export
encode_dcf <- S7::new_generic("encode_dcf", "x")

#' @export
decode_dcf <- S7::new_generic("decode_dcf", "x")

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

#' @export
method(decode_dcf, class_any) <- function(x, ...) {
  eval(parse(text = x), envir = baseenv())
}
