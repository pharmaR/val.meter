.state <- local({
  raise <- FALSE

  raise_derive_errors <- function(value = TRUE) {
    raise <<- value
    once_on_task_callback("raise", raise <<- FALSE)
  }

  environment()
})

once_on_task_callback <- function(name, expr, envir = parent.frame()) {
  addTaskCallback(
    name = paste0(packageName(), "-clear-trace-", name),
    f = function(...) {
      eval(expr, envir = envir)
      FALSE  # don't persist after next callback
    }
  )
}

get_package_boundary_call <- function(calls = sys.calls()) {
  for (i in seq_len(sys.nframe())) {
    if (identical(parent.env(sys.frame(i)), topenv())) {
      return(sys.frame(i))
    }
  }

  sys.frame()
}

cnd_type <- function(class = NULL, cnd = "error") {
  prefix <- gsub(".", "_", packageName(), fixed = TRUE)
  c(
    sprintf("%s_%s_%s", prefix, class, cnd),
    paste(prefix, cnd, sep = "_")
  )
}

err <- function(
  ...,
  data = list(),
  class = NULL,
  .envir = parent.frame()
) {
  args <- data
  args$message <- as.character(list(...))
  args$call <- get_package_boundary_call()
  args$class <- cnd_type(class)
  args$.envir <- .envir
  do.call(cli::cli_abort, args)
}

as_pkg_data_derive_error <- function(x, ...) {
  after <- match(cnd_type(), class(x), nomatch = 1L) - 1L
  class(x) <- append(class(x), cnd_type("derive")[[1L]], after = after)
  new_data <- list(...)
  x[names(new_data)] <- new_data
  x
}
