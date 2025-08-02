#' Convert an object into a `DCF`-formatted string
#' 
#' @param x An object to convert
#' @param ... Additional arguments unused
#' 
#' @returns A `DCF`-formatted string
#' 
#' @export
to_dcf <- S7::new_generic("to_dcf", "x")

#' Parse an object from a `DCF`-formatted string
#' 
#' @param x `character(n)` `DCF` vector
#' @param to A `S7::S7_object` class to convert into
#' @param ... Additional arguments unused
#' 
#' @returns An object of type `to`
#' 
#' @export
from_dcf <- S7::new_generic("from_dcf", c("x", "to"))

method(to_dcf, class_any) <- function(x, ...) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

method(to_dcf, class_atomic) <- function(x, ...) {
  x <- unclass(x)
  attributes(x) <- attributes(x)[intersect(names(attributes(x)), "names")]
  paste(deparse(x), collapse = " ")
}

method(to_dcf, class_integer) <- function(x, ...) {
  paste(deparse(as.numeric(x), width.cutoff = 500L), collapse = " ")
}

#' @include utils_s7.R
method(from_dcf, list(class_character, S7::new_S3_class("S7_class"))) <-
  function(x, to) {
    class_name <- paste(c(to@package, to@name), collapse = "::")
    from_dcf(x, structure(list(), class = c(class_name, "S7_object")))
  }

method(from_dcf, list(class_character, class_any)) <-
  function(
      x,
      to,
      ...,
      eval = TRUE,
      fragment = c("key-values", "key-value", "value")) {
    fragment <- match.arg(fragment)
    switch(fragment,
      "key-values" = {
        con <- textConnection(x)
        x <- as.data.frame(read.dcf(con, all = TRUE))

        eval <- eval & endsWith(colnames(x), "@R")
        colnames(x) <- sub("@R$", "", colnames(x))

        for (i in seq_len(ncol(x))) {
          name <- colnames(x)[[i]]
          x[[name]] <- lapply(
            x[[name]],
            from_dcf,
            fragment = "value",
            eval = eval[[i]]
          )
        }

        x
      },
      "key-value" = {
        con <- textConnection(x)
        key <- scan(con, what = character(1L), n = 1, sep = ":")
        value_str <- paste0(readLines(con), collapse = "\n")
        eval <- eval && endsWith(key, "@R")
        key <- sub("@R$", "", key)
        value <- from_dcf(value_str, fragment = "value", eval = eval)
        x <- list(value)
        names(x) <- key
        x
      },
      "value" = {
        if (eval) eval(parse(text = x), envir = topenv()) else x
      }
    )
  }
