#' @keywords internal
#'
#' @importFrom utils packageName packageVersion
#' @importFrom stats rpois runif
#' 
#' @import cli
#' @import options
#' @import S7
"_PACKAGE"

#' Words for Package Names
#'
#' A dataset containing positive words and their associated parts of speech.
#' Words are selected from the `tidytext` `parts_of_speech` dataset, filtered
#' for words with positive associations.
#'
#' @format A data frame with 1394 rows and 3 variables:
#' \describe{
#'   \item{word}{words with positive sentiment}
#'   \item{pos}{part of speech}
#'   \item{weight}{an entirely non-scientific weighting to use when sampling
#'     words to use for package names. Gives extra weight to some science and
#'     statistical words and R-isms that would make for fun surprises.}
#' }
#' 
#' @source tidytext
"pkg_words"