test_that("logs are captured during package data evaluation", {
  old <- options(val.meter.logs = TRUE)
  on.exit(options(old))

  impl_data(
    "logs_test_name_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      cat("text\n")
      message("message")
      warning("warning")
      cli::cat_line(cli::col_blue("blue"))
      nchar(pkg$name)
    }
  )

  # expect that we have produced some logs
  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_no_error(p$logs_test_name_character_count)
  expect_true(!is.null(logs <- p@logs[["logs_test_name_character_count"]]))

  # expect to find our output in our logs
  expect_true(is.character(text_logs <- format(logs, style = "text")))
  expect_match(text_logs, "text")
  expect_match(text_logs, "\\bmessage\\b")
  expect_match(text_logs, "\\bWarning in")
  expect_match(text_logs, "\\bwarning\\b")

  # expect that we can format our logs as ansi strings
  expect_true(is.character(ansi_logs <<- format(logs, style = "ansi")))
  expect_true(nchar(ansi_logs) > nchar(text_logs))

  # expect that we can produce an html div from our logs
  expect_s3_class(html_logs <- format(logs, style = "html"), "shiny.tag")
})

test_that("logging disabled does not intercept error messages", {
  old <- options(val.meter.logs = FALSE)
  on.exit(options(old))

  impl_data(
    "logs_test_name_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      stop("error!!")
      cli::cat_line(cli::col_blue("blue"))
      nchar(pkg$name)
    }
  )

  p <- pkg(test_path("..", "fixtures", "pkg.local.source"))
  expect_s3_class(p$logs_test_name_character_count, "error")
  expect_equal(p$logs_test_name_character_count$body, "error!!")
})

test_that("logging enabled does not intercept error messages", {
  old <- options(val.meter.logs = TRUE)
  on.exit(options(old))

  impl_data(
    "logs_test_name_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      stop("error!!")
      cli::cat_line(cli::col_blue("blue"))
      nchar(pkg$name)
    }
  )

  p <- pkg(test_path("..", "fixtures", "pkg.local.source"))
  expect_s3_class(p$logs_test_name_character_count, "error")
  expect_equal(p$logs_test_name_character_count$body, "error!!")
})

test_that("logging can be disabled by global option", {
  old <- options(val.meter.logs = FALSE)
  on.exit(options(old))

  impl_data(
    "logs_test_name_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      cat("text\n")
      message("message")
      warning("warning")
      cli::cat_line(cli::col_blue("blue"))
      nchar(pkg$name)
    }
  )

  # expect that we have produced some logs
  p <- pkg(mock_resource(package = "test", version = "1.2.3"))
  expect_message({
    expect_warning({
      expect_output(p$logs_test_name_character_count)
    })
  })

  expect_true(is.null(logs <- p@logs[["logs_test_name_character_count"]]))
})

test_that("logging captures logs for the deepest evaluated metric", {
  # in situations where one metric requires the evaluation of another metric,
  # the output should be attributed to the lowest evaluated metric on the call
  # stack

  old <- options(val.meter.logs = TRUE)
  on.exit(options(old))

  impl_data(
    "logs_random_word",
    metric = TRUE,
    class = class_character,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      cat("inner_cat\n")
      message("inner_message")
      chars <- sample(letters, round(runif(1, 20, 40)), replace = TRUE)
      paste(chars, collapse = "")
    }
  )

  impl_data(
    "logs_random_word_character_count",
    metric = TRUE,
    class = class_integer,
    overwrite = TRUE,
    quiet = TRUE,
    function(pkg, resource, field, ...) {
      cat("outer_cat1\n")
      message("outer_message1")
      warning("outer_warning1")
      cli::cat_line(cli::col_blue("blue"))
      out <- nchar(pkg$logs_random_word)
      cat("outer_cat2\n")
      message("outer_message2") # after returning to outer evaluation
      out
    }
  )

  # expect that logs were captured
  p1 <- pkg(mock_resource(package = "test", version = "1.2.3"))
  p2 <- pkg(mock_resource(package = "test", version = "1.2.3"))

  # first evaluate outer metric, which will call inner metric
  expect_silent(p1$logs_random_word_character_count)
  expect_silent({
    p2$logs_random_word
    p2$logs_random_word_character_count
  })

  # expect logs to be execution order independent
  expect_identical(
    p1@logs$logs_random_word_character_count,
    p2@logs$logs_random_word_character_count
  )
  expect_identical(
    p1@logs$logs_random_word,
    p2@logs$logs_random_word
  )

  # expect that inner logs capture messages from inner metric
  expect_length(p1@logs$logs_random_word, 2L)
  expect_match(p1@logs$logs_random_word[[1L]], "inner_cat")
  expect_match(
    conditionMessage(p1@logs$logs_random_word[[2L]]),
    "inner_message"
  )

  # expect that outer logs capture its output after returning from inner capture
  expect_length(p1@logs$logs_random_word_character_count, 5L)
  expect_match(p1@logs$logs_random_word_character_count[[4L]], "outer_cat2")
  expect_match(
    conditionMessage(p1@logs$logs_random_word_character_count[[5L]]),
    "outer_message2"
  )
})
