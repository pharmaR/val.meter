#' Construct a `character` subclass with a restricted set of values
#' @keywords internal
enum_vector <- function(name, ..., enum = c(), error) {
  if (missing(error)) {
    error <- "{.cls name} values must be one of accepted values: {.str {enum}}"
  }

  cls <- new_class(
    name = name,
    ...,
    constructor = function(...) {
      n <- ...length()
      value <- if (n == 0L) enum
      else if (n == 1L && identical(..1, TRUE)) enum
      else if (n == 1L && identical(..1, FALSE)) character(0L)
      else as.character(c(...))
      S7::new_object(value)
    },
    validator = function(self) {
      if (!all(self %in% enum)) fmt(error)
    }
  )

  method(convert, list(NULL, cls)) <-
    function(from, to, ...) to(from)

  method(convert, list(class_logical, cls)) <-
    function(from, to, ...) to(from)

  method(convert, list(class_character, cls)) <-
    function(from, to, ...) to(from)

  cls
}

#' Roxygenize a class object derived with `enum_vector`
#' @keywords internal
roxygenize_enum_vector <- function(class) {
  enum <- with(environment(class@constructor), enum)
  paste0(
    "# Accepted Values\n\n",
    paste0(" - `\"", enum, "\"`\n", collapse = "")
  )
}
