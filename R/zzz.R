.onLoad <- function(libname, pkgname) {
  S7::S7_on_load()
  s3_register("knitr::knit_print", "evaluate_evaluation")
}

.onUnload <- function(libpath) {
  S7::S7_on_unload()
}

S7::S7_on_build()
