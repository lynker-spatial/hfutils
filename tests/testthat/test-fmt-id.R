test_that("hf_fmt_id is scientific-notation safe", {
  # The bug this guards: as.character(22000000) -> "2.2e+07", which fails every
  # downstream integer-string join and silently drops the record.
  expect_identical(hf_fmt_id(22000000), "22000000")
  expect_identical(hf_fmt_id(2.2e7), "22000000")
  expect_identical(hf_fmt_id("2.2e+07"), "22000000")   # re-normalize a corrupted id
  expect_identical(hf_fmt_id(c(22000000, 5, 100000)), c("22000000", "5", "100000"))
})

test_that("hf_fmt_id preserves split .part suffixes and NA", {
  expect_identical(hf_fmt_id("123.10"), "123.10")   # must NOT round-trip .10 -> .1
  expect_identical(hf_fmt_id("123.1"), "123.1")
  expect_identical(hf_fmt_id(NA), NA_character_)
  expect_identical(hf_fmt_id(c("22000000", NA)), c("22000000", NA))
})
