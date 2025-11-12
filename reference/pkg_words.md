# Words for Package Names

A dataset containing positive words and their associated parts of
speech. Words are selected from the `tidytext` `parts_of_speech`
dataset, filtered for words with positive associations.

## Usage

``` r
pkg_words
```

## Format

A data frame with 1394 rows and 3 variables:

- word:

  words with positive sentiment

- pos:

  part of speech

- weight:

  an entirely non-scientific weighting to use when sampling words to use
  for package names. Gives extra weight to some science and statistical
  words and R-isms that would make for fun surprises.

## Source

tidytext
