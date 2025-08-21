random_pkg_name_styles <- local({
  word <- function(x, n = 1, nprob = 1) {
    with(pkg_words, {
      sample(word[pos %in% x], sample(n, 1), prob = weight[pos %in% x])
    })
  }

  prefix <- function(...) word("Prefix", ...)
  adjective <- function(...) word("Adjective", ...)
  noun <- function(...) word("Noun", ...)
  verb <- function(...) word("Verb", ...)
  suffix <- function(...) word("Suffix", ...)

  list(
    "generic-an" = structure(weight = 1, function(...) {
      paste0(
        collapse = sample(c("", "."), 1L, prob = c(5, 1)),
        c(
          tolower(adjective(1:2, c(10, 3))),
          tolower(noun())
        )
      )
    }),
    "generic-xn" = structure(weight = 1, function(...) {
      paste0(
        collapse = sample(c("", "."), 1L, prob = c(5, 1)),
        c(
          tolower(word(c("Verb", "Noun"))),
          tocamel(noun())
        )
      )
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
        paste(
          collapse = "",
          sample(
            letters,
            sample(1:3, 1, prob = c(3, 10, 1))
          )
        ),
        tolower(suffix())
      )
    }),
    "allcaps" = structure(weight = 0.1, function(...) {
      styles <- names(random_pkg_name_styles)
      styles <- styles[startsWith(styles, "generic")]
      toupper(random_pkg_name(styles = styles))
    }),
    "r-cute" = structure(weight = 0.5, function(...) {
      with(pkg_words, {
        gsub("er$", "r", sample(word[endsWith(word, "er")], 1))
      })
    }),
    "r-cap" = structure(weight = 0.5, function(...) {
      with(pkg_words, {
        sub("r", "R", sample(word[grepl("r", word)], 1))
      })
    }),
    "r-prefix" = structure(
      weight = 0.1,
      function(..., styles = names(random_pkg_name_styles)) {
        styles <- styles[!startsWith(styles, "r-")]
        paste0(sample(c("r", "R"), 1), random_pkg_name(styles = styles))
      }
    ),
    "r-suffix" = structure(
      weight = 0.1,
      function(..., styles = names(random_pkg_name_styles)) {
        styles <- styles[!startsWith(styles, "r-")]
        paste0(random_pkg_name(styles = styles), "R")
      }
    )
  )
})

#' @importFrom utils head
random_pkg_name <- function(n = 1, styles) {
  if (missing(styles) || is.null(styles)) {
    styles <- names(random_pkg_name_styles)
  }

  idx <- names(random_pkg_name_styles) %in% styles
  prob <- vnapply(random_pkg_name_styles, attr, "weight")[idx]

  attempts <- 0L
  chars <- charToRaw(paste0(collapse = "", letters, LETTERS))
  pkg_names <- character(0L)
  while ((n_rem <- n - length(pkg_names)) > 0) {
    new_names <- if (attempts < 2) {
      # first try a couple times to generate cute names
      vcapply(
        sample(styles, size = n_rem, prob = prob, replace = TRUE),
        function(style) random_pkg_name_styles[[style]]()
      )
    } else {
      # if we are running out of cute package names, fall back to gibberish
      vcapply(
        sample(8:32, size = n_rem, replace = TRUE),
        function(nchar) rawToChar(sample(chars, size = nchar, replace = TRUE))
      )
    }

    pkg_names <- unique(c(pkg_names, new_names))
  }

  utils::head(pkg_names, n)
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

random_pkg_graph <- function(vnames) {
  n <- length(vnames)
  g <- igraph::sample_gnp(n, min(20 / n, 1))
  g <- igraph::as_directed(g, mode = "acyclic")
  igraph::V(g)$name <- vnames
  igraph::E(g)$type <- sample(
    c("Depends", "Imports", "LinkingTo", "Suggests"),
    size = length(igraph::E(g)),
    prob = c(2, 10, 1, 10),
    replace = TRUE
  )

  g
}

random_pkg_igraph_deps <- function(g, v) {
  e <- igraph::incident_edges(g, v, mode = "out")[[1]]
  data.frame(
    type = c("Depends", e$type),
    package = c("R", igraph::head_of(g, e)$name),
    version = "*"
  )
}

random_pkg_naive_deps <- function(pkg_name, ..., cohort) {
  deps <- list()
  cohort <- setdiff(cohort, resource@package)
  deps$Depends <- c("R", sample(cohort, min(rpois(1, 0.5), length(cohort))))
  cohort <- setdiff(cohort, deps$Depends)
  deps$LinkingTo <- sample(cohort, min(rpois(1, 0.5), length(cohort)))
  cohort <- setdiff(cohort, deps$LinkingTo)
  deps$Imports <- sample(cohort, min(rpois(1, 2), length(cohort)))
  cohort <- setdiff(cohort, deps$Imports)
  deps$Suggests <- sample(cohort, min(rpois(1, 2), length(cohort)))

  data.frame(
    type = rep(names(deps), times = viapply(deps, length)),
    package = unlist(deps, use.names = FALSE),
    version = "*"
  )
}
