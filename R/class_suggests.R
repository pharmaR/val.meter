suggests <- class_suggests <- new_class(
  "suggests",
  parent = class_character
)

method(convert, list(class_character, class_suggests)) <-
  function(from, to) to(from)

method(convert, list(NULL, class_suggests)) <-
  function(from, to) convert(character(0L), to)
