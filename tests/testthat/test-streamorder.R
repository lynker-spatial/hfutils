test_that("get_streamorder computes Strahler order", {
  # two headwaters join -> order 2
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("1", "2", "3"), flowpath_toid = c("3", "3", "0"))), c(1L, 1L, 2L))
  # linear chain stays order 1
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("a", "b", "c"), flowpath_toid = c("b", "c", "0"))), c(1L, 1L, 1L))
  # (1,2)->4 [tie->2]; 3->5 [1]; (4,5)->6 [max 2, no tie -> 2]
  so <- get_streamorder(data.frame(
    flowpath_id  = c("1", "2", "3", "4", "5", "6"),
    flowpath_toid = c("4", "4", "5", "6", "6", "0")))
  expect_equal(so, c(1L, 1L, 1L, 2L, 1L, 2L))
  # dangling/terminal toid treated as outlet (no error)
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("1", "2"), flowpath_toid = c("2", "tnx-9"))), c(1L, 1L))
})

test_that("get_streamorder is linear at scale (mega-basin guard, not O(n^2))", {
  # 120k-reach chain: linear finishes in well under a second; the old
  # character-named-vector version was O(n^2) and took minutes (it stalled the
  # Mississippi's 461k reaches for ~44 min).
  n  <- 120000L
  df <- data.frame(flowpath_id = as.character(1:n),
                   flowpath_toid = as.character(c(2:n, 0)))
  t  <- system.time(so <- get_streamorder(df))[["elapsed"]]
  expect_lt(t, 15)              # generous; linear is ~0.3s
  expect_true(all(so == 1L))    # a single chain is entirely Strahler order 1
})
