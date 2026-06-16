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
