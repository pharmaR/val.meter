# Create an install_resource for a minimal fake installed package (just a
# DESCRIPTION file), cleaned up when `env` exits.
local_installed_resource <- function(package, env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  writeLines(
    c(paste("Package:", package), "Version: 1.0.0"),
    file.path(path, "DESCRIPTION")
  )
  install_resource(package = package, version = "1.0.0", path = path)
}
