#' Package Metric Classes
#'
#' Package metrics are like any other package data, but have additional
#' metadata and are constrained to only atomic types.
#'
#' @name pkg_metric
local({
  classes <- lapply(class_atomic$classes, function(atomic) {
    name <- atomic$class
    class_name <- paste0("pkg_metric_", name)

    # create metric atomic class
    metric_atomic_class <- new_class(class_name, parent = atomic)

    # implement conversion function
    method(convert, list(atomic, metric_atomic_class)) <-
      function(from, to) to(from)

    # assign metric class in package environment
    assign(class_name, metric_atomic_class, envir = topenv())

    metric_atomic_class
  })

  pkg_metric <<- do.call(new_union, classes)
})
