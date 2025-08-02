library(tidytext)

df <- merge(
  tidytext::parts_of_speech,
  tidytext::get_sentiments(),
  by = "word"
)

# A curated list of common CRAN prefixes, with some restrictions:
#   - brand name prefixes omitted
#   - avoid prefixes which might compose into offensive phrases
#   - weights attempt to roughly match with CRAN prevalance
#   - extra weight given to words that might be relevant to regulated
#     industries to create recognizable patterns
#   - extra weight given for quirkiness (eg tidy-, -plyr)
cran_words <- as.data.frame(matrix(
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c(), c("word", "pos", "weight")),
  data = c(
    "bayes", "Adjective", 5,
    "bayesian", "Adjective", 5,
    "bioc", "Adjective", 3,
    "bio", "Prefix", 3,
    "biology", "Noun", 3,
    "carto", "Adjective", 3,
    "cartography", "Noun", 3,
    "causal", "Adjective", 3,
    "chem", "Adjective", 3,
    "chemo", "Prefix", 3,
    "chemistry", "Noun", 3,
    "clima", "Adjective", 3,
    "climate", "Noun", 3,
    "clin", "Adjective", 5,
    "clinical", "Adjective", 5,
    "cluster", "Noun", 3,
    "code", "Adjective", 3,
    "code", "Noun", 3,
    "cohort", "Noun", 3,
    "color", "Noun", 3,
    "comp", "Adjective", 3,
    "cond", "Adjective", 3,
    "corr", "Adjective", 3,
    "count", "Noun", 3,
    "cov", "Adjective", 5,
    "cox", "Adjective", 5,
    "cpp", "Adjective", 3,
    "cpp11", "Adjective", 3,
    "crop", "Noun", 3,
    "cyclo", "Adjective", 3,
    "data", "Noun", 10,
    "deep", "Adjective", 5,
    "desc", "Noun", 3,
    "diff", "Noun", 3,
    "dist", "Noun", 3,
    "distribution", "Noun", 3,
    "distance", "Noun", 3,
    "dyn", "Prefix", 3,
    "easy", "Adjective", 10,
    "eco", "Prefix", 10,
    "epi", "Prefix", 5,
    "fast", "Adjective", 10,
    "flex", "Adjective", 5,
    "forecast", "Noun", 3,
    "futile", "Adjective", 3,
    "fuzzy", "Adjective", 5,
    "generalized", "Adjective", 3,
    "geno", "Adjective", 3,
    "genomic", "Adjective", 3,
    "geo", "Adjective", 10,
    "gg", "Prefix", 30,
    "glm", "Adjective", 10,
    "graph", "Noun", 5,
    "grid", "Noun", 3,
    "health", "Noun", 5,
    "healthy", "Adjective", 5,
    "read", "Verb", 10,
    "robust", "Adjective", 10,
    "roxy", "Prefix", 10,
    "safety", "Noun", 10,
    "plyr", "Suffix", 5,
    "sample", "Adjective", 5,
    "sampling", "Noun", 5,
    "seq", "Prefix", 5,
    "sf", "Prefix", 3,
    "shiny", "Adjective", 20,
    "sim", "Prefix", 10,
    "smart", "Adjective", 5,
    "smooth", "Adjective", 3,
    "soil", "Noun", 3,
    "space", "Noun", 3,
    "sparse", "Adjective", 10,
    "spatial", "Adjective", 20,
    "spectral", "Adjective", 10,
    "stat", "Adjective", 3,
    "strat", "Adjective", 3,
    "surv", "Prefix", 10,
    "survival", "Noun", 10,
    "survey", "Noun", 10,
    "table", "Noun", 10,
    "tensor", "Noun", 10,
    "test", "Noun", 10,
    "tidy", "Prefix", 20,
    "tidy", "Adjective", 20,
    "tiny", "Adjective", 10,
    "time", "Noun", 10,
    "topo", "Prefix", 3,
    "tree", "Noun", 5,
    "val", "Prefix", 20,
    "validated", "Adjective", 5,
    "vis", "Prefix", 10,
    "web", "Prefix", 20,
    "weighted", "Adjective", 10,
    "word", "Noun", 10
  )
))

# correct column types
cran_words$weight <- as.integer(cran_words$weight)

# avoid accidentally offensive package names by restricting to positive words
is_positive <- df$sentiment != "negative"

# we only really need adjectives and nouns
is_useful_pos <- df$pos %in% c("Adjective", "Noun", "Verb")

# avoid hyphenated words and others, eg "well-balanced"
has_sym <- grepl("[^[:alpha:]]", df$word)

# filter and drop sentiment column
pkg_words <- df[is_useful_pos & is_positive & !has_sym, 1:2]
pkg_words[, "weight"] <- 1L  # add default weight column

pkg_words <- rbind(pkg_words, cran_words)
pkg_words <- pkg_words[order(pkg_words$word), ]
rownames(pkg_words) <- NULL

# of course some prefixes are much more prevalent in R
usethis::use_data(pkg_words, overwrite = TRUE)
