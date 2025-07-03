next_id <- local({
  last_id <- 0L
  function() {
    last_id <<- last_id + 1L
  }
})

ns_tmp_root <- function() {
  dir_create(file.path(tempdir(), packageName()))
}

pkg_dir <- function(pkg, ..., .root = opt("logs")) {
  dir_create(file.path(.root, "pkg", pkg$name, ...))
}

resource_dir <- function(x, ..., .id = x@id, .root = opt("logs")) {
  dir_create(file.path(.root, "rsrc", .id, ...))
}

dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}
