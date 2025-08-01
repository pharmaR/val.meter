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

#' Automatically convert properties to desired type upon object construction
#'
#' @note used for [`S7::new_property`]`(setter = )` to handle conversion on
#'   creation
#'
#' To allow "duck-typed" S7 class construction, we can use this function as
#' our [`S7::new_property`]`(setter = )` and it will implicitly try to convert
#' any new value to our desired type, raising an error if the input type can't
#' be converted.
#' 
#' @param self,value see [`S7::new_property`]`(setter = )` documentation for
#'   function signature details.
#'
#' @keywords internal
#' @noRd
setter_try_from <- function(...) {
  function(self, value) {
    # we need to grab our S7 class information from the parent call
    self_object <- attr(with(sys.frame(-1L), object), "S7_class")
    prop_name <- with(sys.frame(-1L), name)
    prop <- self_object@properties[[prop_name]]
    
    # then we can generically try to convert to our required class, returning
    # the original value if we couldn't convert (which will throw a more
    # informative error) when S7 tries to validate the object
    S7::prop(self, prop_name) <- tryCatch(
      convert(value, prop$class),
      error = function(e) value
    )
    
    self
  }
}

#' Automatically convert an object during construction
#' 
#' @note used for [`S7::new_class`]`(constructor = )` to handle conversion
#'   upon object construction.
#'   
#' By default, injects an argument into the default constructor called `.from`,
#' which will be used in an attempt to convert to the object on construction.
#' 
#' @examples
#' my_integer <- new_class(
#'   "my_integer",
#'   constructor = constructor_try_from(),
#'   properties = list(
#'     value = new_property(
#'       class_integer,
#'       default = 0L
#'     )
#'   )
#' )
#' 
#' # this method will be used implicitly when we try to create with .from
#' method(convert, list(class_character, my_integer)) <- 
#'   function(from, to, ...) {
#'     to(value = as.integer(from))
#'   }
#'   
#' my_integer(.from = "100")
#'
#' # note that the default args will take precedence, masking any properties
#' # initialized during conversion
#' my_integer(.from = "100", value = 3L)
#' 
#' @keywords internal
#' @noRd
constructor_try_from <- function(...) {
  # NOTE:
  #   This function does some magic to trick S7 into first trying to convert
  #   an input into the desired object type. To both add our own conversion
  #   constructor _and_ leverage `S7`'s default constructor we need to create
  #   a mimic of our class object so we can still fall back to the default
  #   constructor function. 
  
  # create our class mimic, copying all the same class parameters, but tossing
  # out this constructor.
  self <- sys.frame(-1L)
  self_args <- list()
  for (name in names(self)) {
    if (identical(name, "constructor")) next
    self_args[[name]] <- self[[name]]
  }
  
  default_constructor <- do.call(new_class, self_args)
  
  constructor <- function(.from) {
    # other args will come from the default constructor
    args <- list()
    arg_names <- setdiff(names(match.call(expand.dots = TRUE)[-1]), ".from")
    for (name in arg_names) args[[name]] <- environment()[[name]]
    
    class <- sys.function()
    if (!missing(.from)) {
      # attempt conversion, overlaying parameter args
      return(tryCatch(
        valid_eventually(convert(.from, class), function(object) {
          props(object) <- args
          object
        }), 
        error = function(e) do.call(default_constructor, args)
      ))
    } else {
      return(do.call(default_constructor, args))
    }
    
    # never end up here, we just need to trick S7
    new_object(class@parent)
  }
  
  formals(constructor) <- append(
    formals(default_constructor),
    formals(constructor)
  )
  
  constructor
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

#' Generate a graph of class relations
#' 
#' @keywords internal
#' @noRd
class_graph <- function(x) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("igraph is required to generate a class graph")
  }
  
  if (is.character(x)) {
    x <- getNamespace(x)
  }
  
  if (is.environment(x)) {
    x <- Filter(function(xi) inherits(xi, "S7_class"), as.list(x))
    names(x) <- vcapply(x, class_desc)
    x <- x[!duplicated(names(x))]
  }
  
  x_parents <- vcapply(x, function(xi) {
    if (!is.null(xi@parent) && !identical(xi@parent, S7::S7_object)) {
      class_desc(xi@parent)
    } else {
      NA_character_
    }
  })
  
  has_parent <- !is.na(x_parents) & x_parents %in% names(x)
  
  g <- igraph::make_empty_graph()
  
  g <- igraph::add_vertices(
    g, 
    length(x),
    attr = list(
      name = names(x),
      class = vcapply(x, S7::prop, "name"),
      abstract = vlapply(x, S7::prop, "abstract"),
      properties = lapply(x, S7::prop, "properties")
    )
  )
  
  g <- igraph::add_edges(g, rbind(
    tail = names(x)[has_parent],
    head = as.character(x_parents[has_parent])
  ))

  g
}
