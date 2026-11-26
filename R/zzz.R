.onLoad <- function(lib, pkg) {
  S7::S7_on_load()
  s3_register("knitr::knit_print", "evaluate_evaluation")
}

# Unclear why this is needed, but without it we get an R CMD check NOTE
utils::globalVariables(c("properties"))

# Remove the placeholders left in the namespace by `method<-()` (S7 >= 1.0.0)
S7::S7_on_build()
