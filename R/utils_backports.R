#' Backport of tools md5sum bytes encoding
#'
#' @keywords internal
#' @noRd
md5sum <- function(x) {
  if (packageVersion("tools") >= "4.5.1") {
    tools::md5sum(bytes = charToRaw(x))
  } else {
    writeLines(x, f <- tempfile())
    on.exit(file.remove(f))
    tools::md5sum(files = f)
  }
}
