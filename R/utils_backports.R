#' Backport of tools md5sum bytes encoding
#' 
#' @keywords internal
#' @noRd
md5sum <- function(bytes) {
  if (packageVersion("tools") >= "4.5.1") {
    tools::md5sum(bytes = bytes)
  } else {
    .Call(getNamespace("tools")[["C_Rmd5"]], bytes)
  }
}