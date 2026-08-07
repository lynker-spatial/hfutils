if (requireNamespace("spelling", quietly = TRUE)) {
  # error = TRUE so a new misspelling fails the suite instead of scrolling past
  # in the log; genuine technical terms belong in inst/WORDLIST.
  spelling::spell_check_test(vignettes = TRUE, error = TRUE,
    skip_on_cran = TRUE)
}
