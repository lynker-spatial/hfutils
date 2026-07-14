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

test_that("get_pathlength sums downstream lengths, excluding the reach itself", {
  x <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"),
    lengthkm = c(5, 5, 5), stringsAsFactors = FALSE)
  # outlet (3) = 0; 2 = len(3) = 5; 1 = len(2) + len(3) = 10
  expect_equal(get_pathlength(x, length = "lengthkm"), c(10, 5, 0))
})

test_that("get_pathlength follows the mainstem past a confluence", {
  # 1 -> 3, 2 -> 3, 3 -> 4 (outlet); the tributary sees only 3 then 4
  y <- data.frame(flowpath_id = c("1", "2", "3", "4"),
    flowpath_toid = c("3", "3", "4", "0"),
    lengthkm = c(2, 9, 4, 3), stringsAsFactors = FALSE)
  # 4 = 0; 3 = len(4) = 3; 1 = len(3)+3 = 7; 2 = len(3)+3 = 7
  expect_equal(get_pathlength(y, length = "lengthkm"), c(7, 7, 3, 0))
})

test_that("get_pathlength rejects a cyclic network", {
  cyc <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1"),
    lengthkm = c(1, 1), stringsAsFactors = FALSE)
  expect_error(get_pathlength(cyc, length = "lengthkm"), "cycle")
})

test_that("get_streamlevel: mainstem is 1, a tributary is 2", {
  df <- data.frame(flowpath_id = c("1", "2", "3", "4"),
    flowpath_toid = c("0", "1", "1", "3"),
    levelpath = c("A", "B", "A", "A"), stringsAsFactors = FALSE)
  # level path A drains out (1); B empties into A (2)
  expect_equal(get_streamlevel(df), c(1, 2, 1, 1))
})

test_that("get_streamlevel increments for a tributary of a tributary", {
  df <- data.frame(flowpath_id = c("1", "2", "3", "4", "5"),
    flowpath_toid = c("0", "1", "1", "2", "3"),
    levelpath = c("A", "B", "A", "C", "A"), stringsAsFactors = FALSE)
  # A terminal (1); B -> A (2); C -> B (3)
  expect_equal(get_streamlevel(df), c(1, 2, 1, 3, 1))
})

test_that("get_pfafstetter reproduces reference basin codes (746-reach basin)", {
  # Known-answer fixture: the NHDPlus New Hope Creek basin (public domain), with
  # precomputed total_da_sqkm / topo_sort / levelpath inputs and the reference
  # 2-level Pfafstetter codes. Exact match incl. NA placement (reaches whose
  # sub-basin is deeper than max_level are NA).
  fx <- readRDS(test_path("fixtures", "pfaf_oracle.rds"))
  g2 <- get_pfafstetter(fx, id = "id", toid = "toid", max_level = 2)
  expect_equal(g2, fx$pf_level_2)
  # the top-basin (level-1) digit is the leading digit of the level-2 code
  lead <- ifelse(is.na(g2), NA_real_, ifelse(g2 >= 10, floor(g2 / 10), g2))
  expect_equal(lead, fx$pf_level_1)
})

test_that("get_pathlength matches an independent downstream walk on a random DAG", {
  set.seed(1)
  n <- 40L
  # each reach drains to a strictly higher index (guarantees a DAG); last is the
  # outlet. sample.int(n - i, 1) is safe for the single-choice (i == n-1) case.
  toid <- integer(n)
  for (i in seq_len(n - 1L)) toid[i] <- i + sample.int(n - i, 1L)  # in (i+1)..n
  x <- data.frame(flowpath_id = as.character(seq_len(n)),
    flowpath_toid = as.character(toid),
    lengthkm = round(runif(n, 0.1, 20), 3), stringsAsFactors = FALSE)

  # independent reference: walk each reach's unique downstream path to the
  # terminus, summing the length of every reach below it (excluding itself)
  ref <- vapply(seq_len(n), function(i) {
    tot <- 0; d <- toid[i]
    while (d != 0L) { tot <- tot + x$lengthkm[d]; d <- toid[d] }
    tot
  }, numeric(1))

  expect_equal(get_pathlength(x, length = "lengthkm"), ref)
})
