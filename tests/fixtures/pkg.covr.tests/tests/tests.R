library(pkg.covr.tests)
stopifnot(covered(1) == 2)

# simulate a hanging test using a blocking call that R time limits can't stop
if (nzchar(Sys.getenv("PKG_COVR_TESTS_HANG"))) {
  system("sleep 60")
}
