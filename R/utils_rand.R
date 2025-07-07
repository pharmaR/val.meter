random_pkg_name_styles <- local({
  word <- function(x, n = 1, nprob = 1) with(pkg_words, {
    sample(word[pos %in% x], sample(n, 1), prob = weight[pos %in% x])
  })

  prefix <- function(...) word("Prefix", ...)
  adjective <- function(...) word("Adjective", ...)
  noun <- function(...) word("Noun", ...)
  verb <- function(...) word("Verb", ...)
  suffix <- function(...) word("Suffix", ...)

  list(
    "generic-an" = structure(weight = 1, function(...) {
      paste0(collapse = sample(c("", "."), 1L, prob = c(5, 1)), c(
        tolower(adjective(1:2, c(10, 3))),
        tolower(noun())
      ))
    }),
    "generic-xn" = structure(weight = 1, function(...) {
      paste0(collapse = sample(c("", "."), 1L, prob = c(5, 1)), c(
        tolower(word(c("Verb", "Noun"))),
        tocamel(noun())
      ))
    }),
    "tidy" = structure(weight = 0.5, function(...) {
      paste0(tolower(prefix()), tolower(noun()))
    }),
    "BioC" = structure(weight = 0.2, function(...) {
      paste0(tocamel(word(c("Adjective", "Prefix"))), tocamel(noun()))
    }),
    "ecosystem" = structure(weight = 0.1, function(...) {
      paste(sep = ".", tolower(prefix()), tolower(noun()))
    }),
    "plyr" = structure(weight = 0.1, function(...) {
      paste0(
        paste(collapse = "", sample(
          letters,
          sample(1:3, 1, prob = c(3, 10, 1))
        )),
        tolower(suffix())
      )
    }),
    "allcaps" = structure(weight = 0.1, function(...) {
      styles <- names(random_pkg_name_styles)
      styles <- styles[startsWith(styles, "generic")]
      toupper(random_pkg_name(styles = styles))
    }),
    "r-cute" = structure(weight = 0.5, function(...) with(pkg_words, {
      gsub("er$", "r", sample(word[endsWith(word, "er")], 1))
    })),
    "r-cap" = structure(weight = 0.5, function(...) with(pkg_words, {
      sub("r", "R", sample(word[grepl("r", word)], 1))
    })),
    "r-prefix" = structure(weight = 0.1,
      function(..., styles = names(random_pkg_name_styles)) {
        styles <- styles[!startsWith(styles, "r-")]
        paste0(sample(c("r", "R"), 1), random_pkg_name(styles = styles))
      }
    ),
    "r-suffix" = structure(weight = 0.1,
      function(..., styles = names(random_pkg_name_styles)) {
        styles <- styles[!startsWith(styles, "r-")]
        paste0(random_pkg_name(styles = styles), "R")
      }
    )
  )
})

random_pkg_name <- function(n = 1, styles) {
  if (missing(styles) || is.null(styles)) {
    styles <- names(random_pkg_name_styles)
  }

  idx <- names(random_pkg_name_styles) %in% styles
  prob <- vnapply(random_pkg_name_styles, attr, "weight")[idx]
  pkg_styles <- sample(styles, n, prob = prob, replace = TRUE)
  fns <- unname(random_pkg_name_styles[pkg_styles])
  vcapply(fns, do.call, args = list(styles = styles))
}

random_pkg_version <- function(n = 1) {
  n_parts <- sample(2:3, n, replace = TRUE, prob = c(1, 10))
  a_lambda <- runif(n, min = 1, max = 3)
  b_lambda <- runif(n, min = 2, max = 5)
  c_lambda <- runif(n, min = 2, max = 5)
  c_sep <- sample(c(".", "-"), n, replace = TRUE, prob = c(10, 1))
  paste0(
    rpois(n = n, lambda = a_lambda),
    ".",
    rpois(n = n, lambda = b_lambda),
    ifelse(n_parts > 2, paste0(c_sep, rpois(n = n, lambda = c_lambda)), "")
  )
}
