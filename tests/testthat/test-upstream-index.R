test_that("upstream_index range query equals the graph walk", {
  #        7 (outlet)
  #        |
  #        6
  #       / \
  #      5   2
  #     /|   |
  #    4 3   1
  x <- data.frame(
    flowpath_id   = as.character(1:7),
    flowpath_toid = as.character(c(6, 6, 5, 5, 6, 7, 0)))
  idx <- upstream_index(x)

  expect_equal(attr(idx, "n_bad"), 0L)
  expect_equal(attr(idx, "n_divergences"), 0L)
  expect_equal(attr(idx, "n_outlets"), 1L)
  expect_equal(length(unique(idx$upstream_id)), 7L)   # valid pre-order labelling

  # brute-force upstream walk over the id -> toid graph
  ids <- x$flowpath_id; di <- match(x$flowpath_toid, ids)
  children <- split(seq_along(ids), factor(di, levels = seq_along(ids)))
  walk <- function(r) { acc <- integer(0); stk <- children[[r]]
    while (length(stk)) { h <- stk[1]; stk <- stk[-1]; acc <- c(acc, h); stk <- c(stk, children[[h]]) }; acc }

  for (r in seq_along(ids)) {
    u <- idx$upstream_id[r]; k <- idx$num_upstreams[r]
    rng <- which(idx$upstream_id > u & idx$upstream_id <= u + k)
    expect_setequal(rng, walk(r))
    expect_equal(k, length(walk(r)))
  }
  # spot checks
  expect_equal(idx$num_upstreams[7], 6L)   # outlet sees all others
  expect_equal(idx$num_upstreams[6], 5L)
  expect_equal(idx$num_upstreams[1], 0L)   # headwater
})

test_that("upstream_index treats dangling/terminal toids as outlets", {
  idx <- upstream_index(data.frame(
    flowpath_id = c("1", "2"), flowpath_toid = c("2", "tnx-9")))
  expect_equal(attr(idx, "n_outlets"), 1L)
  expect_equal(idx$num_upstreams, c(0L, 1L))
})

test_that("upstream_index flags a divergence (duplicate id)", {
  div <- data.frame(
    flowpath_id   = c("1", "1", "2"),          # node 1 has two downstreams
    flowpath_toid = c("2", "3", "0"))
  expect_warning(upstream_index(div), "not a tree")
  idx <- suppressWarnings(upstream_index(div))
  expect_gt(attr(idx, "n_divergences"), 0L)
})

test_that("upstream_index errors on a cycle", {
  expect_error(
    upstream_index(data.frame(
      flowpath_id = c("1", "2", "3"), flowpath_toid = c("2", "3", "1"))),
    "cycle")
})

test_that("upstream_index is linear at scale", {
  n <- 120000L
  x <- data.frame(flowpath_id = as.character(1:n),
    flowpath_toid = c(as.character(2:n), "0"))         # single chain
  t <- system.time(idx <- upstream_index(x))[["elapsed"]]
  expect_lt(t, 15)
  expect_equal(attr(idx, "n_bad"), 0L)
  expect_equal(max(idx$num_upstreams), n - 1L)        # outlet sees the whole chain
})
