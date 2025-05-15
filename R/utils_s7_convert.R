#' @include class_tags.R
#' @include class_permissions.R
NULL

method(convert, list(class_character, class_tags)) <-
  function(from, to) to(from)

method(convert, list(NULL, tags)) <-
  function(from, to) convert(character(0L), to)

method(convert, list(class_character, class_permissions)) <-
  function(from, to) to(from)

method(convert, list(NULL, permissions)) <-
  function(from, to) convert(character(0L), to)
