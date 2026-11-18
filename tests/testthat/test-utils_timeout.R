busy_wait <- function(seconds) {
  start <- Sys.time()
  while (difftime(Sys.time(), start, units = "secs") < seconds) {
    NULL
  }
  TRUE
}

impl_slow_data <- function(name, seconds, value = 1L, envir = parent.frame()) {
  impl_data(
    name,
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      busy_wait(seconds)
      value
    }
  )
}

expect_timeout_error <- function(x) {
  testthat::expect_s3_class(x, cnd_type("timeout")[[1L]])
}

test_that("derivations are interrupted after the per-derivation timeout", {
  withr::local_options(val.meter.timeout = 0.5)
  impl_slow_data("timeout_test_slow", seconds = 30)
  impl_slow_data("timeout_test_fast", seconds = 0)

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  elapsed <- system.time(value <- p$timeout_test_slow)[["elapsed"]]

  expect_timeout_error(value)
  expect_lt(elapsed, 5)

  # each derivation gets its own timeout
  expect_identical(p$timeout_test_fast, 1L)

  # time limits don't leak out of the derivation
  expect_no_error(busy_wait(1))
})

test_that("derivations starting after the deadline fail immediately", {
  withr::local_options(val.meter.deadline = Sys.time() - 1)
  impl_slow_data("timeout_test_fast", seconds = 0)

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_timeout_error(p$timeout_test_fast)
})

test_that("deadline accepts seconds since the epoch", {
  withr::local_options(val.meter.deadline = as.numeric(Sys.time()) + 0.5)
  impl_slow_data("timeout_test_slow", seconds = 30)

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_timeout_error(p$timeout_test_slow)
})

test_that("no time limit is imposed by default", {
  impl_slow_data("timeout_test_fast", seconds = 0.2)

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_identical(p$timeout_test_fast, 1L)
  expect_identical(.timeout_state$deadline, Inf)
})

test_that("dependencies share the deadline of the data that requires them", {
  withr::local_options(val.meter.timeout = 0.5)
  impl_slow_data("timeout_test_slow", seconds = 30)
  impl_data(
    "timeout_test_dependent",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      pkg$timeout_test_slow + 1L
    }
  )

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  elapsed <- system.time(value <- p$timeout_test_dependent)[["elapsed"]]

  expect_s3_class(value, "error")
  expect_lt(elapsed, 5)
  expect_timeout_error(p$timeout_test_slow)
})

test_that("outer time limit is restored after a nested derivation", {
  withr::local_options(val.meter.timeout = 1)
  impl_slow_data("timeout_test_fast", seconds = 0)
  impl_data(
    "timeout_test_slow_after_dependency",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      pkg$timeout_test_fast
      busy_wait(30)
      1L
    }
  )

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  elapsed <- system.time({
    value <- p$timeout_test_slow_after_dependency
  })[["elapsed"]]

  expect_timeout_error(value)
  expect_lt(elapsed, 5)
  expect_identical(p$timeout_test_fast, 1L)
})

test_that("derivations that swallow the time limit error still time out", {
  withr::local_options(val.meter.timeout = 0.5)
  impl_data(
    "timeout_test_swallowed",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      tryCatch(busy_wait(30), error = function(e) NULL)
      1L
    }
  )

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_timeout_error(p$timeout_test_swallowed)
})

test_that("time limits apply when logs are captured", {
  withr::local_options(val.meter.timeout = 0.5, val.meter.logs = TRUE)
  impl_slow_data("timeout_test_slow", seconds = 30)

  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_timeout_error(p$timeout_test_slow)
})

test_that("timeout errors round trip through dcf", {
  e <- error("timeout", field = "r_cmd_check")
  expect_timeout_error(e)
  expect_identical(
    to_dcf(e),
    'val.meter::error("timeout", field = "r_cmd_check")'
  )
  expect_timeout_error(eval(str2lang(to_dcf(e))))
})
