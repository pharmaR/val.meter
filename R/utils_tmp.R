ns_tmp_root <- function() {
  dir_create(file.path(tempdir(), packageName()))
}

pkg_tmpdir <- function(pkg, ...) {
  dir_create(file.path(ns_tmp_root(), "pkg", pkg$name, ...))
}

dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}
