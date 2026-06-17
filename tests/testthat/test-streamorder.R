test_that("get_streamorder computes Strahler order", {
  # two headwaters join -> order 2
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("1","2","3"), flowpath_toid = c("3","3","0"))), c(1L,1L,2L))
  # linear chain stays order 1
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("a","b","c"), flowpath_toid = c("b","c","0"))), c(1L,1L,1L))
  # (1,2)->4 [tie->2]; 3->5 [1]; (4,5)->6 [max 2, no tie -> 2]
  so <- get_streamorder(data.frame(
    flowpath_id  = c("1","2","3","4","5","6"),
    flowpath_toid = c("4","4","5","6","6","0")))
  expect_equal(so, c(1L,1L,1L,2L,1L,2L))
  # dangling/terminal toid treated as outlet (no error)
  expect_equal(get_streamorder(data.frame(
    flowpath_id = c("1","2"), flowpath_toid = c("2","tnx-9"))), c(1L,1L))
})
