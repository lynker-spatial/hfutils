## Topology accumulation + DAG behavior.

test_that("accumulate_downstream sums contributions downstream along a chain", {
  x <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"),
    v = c(1, 1, 1), stringsAsFactors = FALSE)
  expect_equal(accumulate_downstream(x, attr = "v"), c(1, 2, 3))
})

test_that("accumulate_downstream merges branches at a confluence", {
  y <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("3", "3", "0"),
    v = c(5, 7, 1), stringsAsFactors = FALSE)
  expect_equal(accumulate_downstream(y, attr = "v"), c(5, 7, 13))
})

test_that("accumulate_downstream rejects a cyclic network", {
  cyc <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1"),
    v = c(1, 1), stringsAsFactors = FALSE)
  expect_error(accumulate_downstream(cyc, attr = "v"), "cycle")
})

test_that("get_hydroseq returns a complete permutation with no gaps", {
  x <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"), stringsAsFactors = FALSE)
  hs <- get_hydroseq(x)
  expect_length(hs, 3L)
  expect_false(anyNA(hs))
  expect_setequal(hs, seq_len(3L))
})

test_that("get_hydroseq is identical for numeric and non-numeric (fp-) ids", {
  xn <- data.frame(flowpath_id = c(1, 2, 3), flowpath_toid = c(2, 3, 0))
  xc <- data.frame(flowpath_id = c("fp-1", "fp-2", "fp-3"),
    flowpath_toid = c("fp-2", "fp-3", "0"), stringsAsFactors = FALSE)
  expect_equal(get_hydroseq(xc), get_hydroseq(xn))
  expect_false(anyNA(get_hydroseq(xc)))
})
