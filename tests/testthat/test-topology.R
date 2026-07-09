test_that("hf_network_is_dag detects cycles; terminals are not edges", {
  chain <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"), stringsAsFactors = FALSE)
  expect_true(hf_network_is_dag(chain))
  # terminal sentinels (0 / NA / >=1e9 / not-in-ids) must not create self-loops
  term <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1000000001"), stringsAsFactors = FALSE)
  expect_true(hf_network_is_dag(term))
  cyc <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1"), stringsAsFactors = FALSE)
  expect_false(hf_network_is_dag(cyc))
})

test_that("hf_network_is_dag accepts a nondendritic (divergent) network", {
  divergent <- data.frame(
    flowpath_id   = c("1", "1", "2", "3"),
    flowpath_toid = c("2", "3", "0", "0"), stringsAsFactors = FALSE)
  expect_true(hf_network_is_dag(divergent))
  diamond <- data.frame(
    flowpath_id   = c("1", "1", "2", "3", "4"),
    flowpath_toid = c("2", "3", "4", "4", "0"), stringsAsFactors = FALSE)
  expect_true(hf_network_is_dag(diamond))
})

test_that("hf_recompute_hydroseq puts outlets lowest (NHD convention)", {
  chain <- data.frame(flowpath_id = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"), stringsAsFactors = FALSE)
  hs <- hf_recompute_hydroseq(chain)$hydroseq
  expect_length(hs, 3L)
  expect_false(anyNA(hs))
  expect_equal(hs[3], min(hs))   # fp 3 is the outlet -> lowest hydroseq
  expect_equal(hs[1], max(hs))   # fp 1 is the headwater -> highest
})

test_that("hf_break_cycles makes a cyclic network acyclic", {
  cyc <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1"), stringsAsFactors = FALSE)
  expect_false(hf_network_is_dag(cyc))
  broken <- suppressMessages(hf_break_cycles(cyc))
  expect_true(hf_network_is_dag(broken))
  expect_equal(nrow(broken), 2L)   # severs an edge, never drops a node
})

test_that("hf_assert_network_dag reports cycle nodes", {
  cyc <- data.frame(flowpath_id = c("1", "2"),
    flowpath_toid = c("2", "1"), stringsAsFactors = FALSE)
  a <- hf_assert_network_dag(cyc)
  expect_false(a$is_dag)
  expect_true(all(c("1", "2") %in% a$cycle_ids))
})
