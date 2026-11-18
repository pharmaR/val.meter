test_that("metric_coerce coerces character metrics to each atomic type", {
  ms <- metrics()

  # integer metric
  int <- metric_coerce("3", ms[["r_cmd_check_error_count"]]@data_class)
  expect_type(int, "integer")
  expect_identical(int, 3L)

  # logical metric
  lgl_true <- metric_coerce("TRUE", ms[["has_website"]]@data_class)
  lgl_false <- metric_coerce("FALSE", ms[["has_website"]]@data_class)
  expect_type(lgl_true, "logical")
  expect_type(lgl_false, "logical")
  expect_identical(lgl_true, TRUE)
  expect_identical(lgl_false, FALSE)

  # double metric
  dbl <- metric_coerce("0.5", ms[["test_line_coverage_fraction"]]@data_class)
  expect_type(dbl, "double")
  expect_identical(dbl, 0.5)
})

test_that("metric_coerce coerces atomic values to character", {
  for (x in list(3L, 0.5, TRUE)) {
    expect_type(metric_coerce(x, class_character), "character")
  }
  expect_identical(metric_coerce(3L, class_character), "3")
  expect_identical(metric_coerce(0.5, class_character), "0.5")
  expect_identical(metric_coerce(TRUE, class_character), "TRUE")
})

test_that("convert package_matrix to metric_data_frame handles atomic types", {
  set.seed(1)
  repo <- suppressWarnings(random_repo(n = 4))
  on.exit(unlink(sub("^file://", "", repo), recursive = TRUE))

  # metric fields are non-standard, so discover them from the PACKAGES DCF
  packages_url <- file.path(
    sub("^file://", "", repo),
    "src", "contrib", "PACKAGES"
  )
  dcf <- paste(readLines(packages_url), collapse = "\n")
  metric_fields <- grep(
    "^Metric/",
    colnames(read.dcf(textConnection(dcf))),
    value = TRUE
  )

  db <- available.packages(
    repos = repo,
    fields = metric_fields,
    filters = list()
  )
  db <- db[!is.na(db[, "Package"]), , drop = FALSE]

  expect_no_error(
    met <- convert(class_package_matrix(db), class_metric_data_frame)
  )

  # logical, double and integer metrics should have been coerced away from
  # their character storage representation
  expect_type(met[["has_website"]], "logical")
  expect_type(met[["test_line_coverage_fraction"]], "double")
  expect_type(met[["r_cmd_check_error_count"]], "integer")
})
