#' Retrieves local CRAN mirrors
#'
#' Retrieves local CRAN mirrors including RStudio CRAN mirror.
#'
#' @inheritParams utils::getCRANmirrors
#'
#' @importFrom utils getCRANmirrors
#' @keywords internal
#' @noRd

# nolint start: object_name_linter.
get_cran_mirrors <- function(all = FALSE, local.only = TRUE) {
  # nolint end
  cran_mirrors <- getCRANmirrors(all = all, local.only = local.only)
  # NOTE: For the time being the POSIT CRAN mirror is being manually
  # added. It's inclusion is justified since it is a true cran mirror and
  # is the default in very populat IDEs, namely RStudio and Positron.
  cran_mirrors <- rbind(
    cran_mirrors,
    data.frame(
      Name = "RStudio IDE",
      Country = character(1),
      City = character(1),
      URL = "https://cran.rstudio.com/",
      Host = character(1),
      Maintainer = character(1),
      OK = integer(1),
      CountryCode = character(1),
      Comment = character(1)
    )
  )

  cran_mirrors
}
