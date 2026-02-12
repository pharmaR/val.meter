.onLoad <- function(lib, pkg) {
  S7::methods_register()
  s3_register("knitr::knit_print", "evaluate_evaluation")
}

# Unclear why this is needed, but without it we get an R CMD check NOTE
utils::globalVariables(c("properties"))
