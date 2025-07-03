#' @include trait_pkg_data.R

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = source_archive_resource,
  function(field, pkg, resource, ...) {
    tools::md5sum(resource@path)
  }
)

impl_data(
  "archive_md5",
  class = class_character,
  for_resource = source_code_resource,
  function(field, pkg, resource, ...) {
    outdir <- tmpdir(pkg, "archive")

    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    setwd(outdir)

    args <- c(resource@path, "--md5", paste0("--user=", packageName()))
    system2("R", c("CMD", "build", args), stdout = FALSE, stderr = FALSE)

    archive <- list.files(outdir)
    unname(tools::md5sum(archive))
  }
)
