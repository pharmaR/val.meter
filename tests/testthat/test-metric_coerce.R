test_that("metric_coerce coerces character metric strings to each atomic type", {
  ms <- metrics()

  # integer metric
  expect_identical(
    metric_coerce("3", ms[["r_cmd_check_error_count"]]@data_class),
    3L
  )

  # logical metric
  expect_identical(
    metric_coerce("TRUE", ms[["has_website"]]@data_class),
    TRUE
  )
  expect_identical(
    metric_coerce("FALSE", ms[["has_website"]]@data_class),
    FALSE
  )

  # double metric
  expect_identical(
    metric_coerce("0.5", ms[["test_line_coverage_fraction"]]@data_class),
    0.5
  )
})

test_that("metric_coerce coerces atomic values to character", {
  expect_identical(metric_coerce(3L, class_character), "3")
  expect_identical(metric_coerce(0.5, class_character), "0.5")
  expect_identical(metric_coerce(TRUE, class_character), "TRUE")
})

test_that("convert(class_package_matrix, class_metric_data_frame) handles all atomic metric types", {
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
