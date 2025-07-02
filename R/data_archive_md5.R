#' @include trait_pkg_data.R

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = archive_source_resource,
  function(field, pkg, resource, ...) {
    browser()
    tools::md5sum(resource@path)
  }
)

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = local_resource,
  function(field, pkg, resource, ...) {
    outdir <- pkg_tmpdir(pkg, "archive")

    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(outdir)

    args <- c(resource@path, "--md5", paste0("--user=", packageName()))
    system2("R", c("CMD", "build", args), stdout = FALSE, stderr = FALSE)

    archive <- list.files(outdir)
    unname(tools::md5sum(archive))
  }
)
