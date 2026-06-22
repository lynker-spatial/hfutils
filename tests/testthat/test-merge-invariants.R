test_that("hf_check_merge_invariants passes a clean merge", {
  fp <- data.frame(flowpath_id = c("fp-1", "fp-2"), vpuid = c("01", "02"),
    total_dasqkm = c(10, 25))
  dv <- data.frame(divide_id = c("cat-1", "cat-2"), vpuid = c("01", "02"),
    areasqkm = c(10, 15))
  nx <- data.frame(nexus_id = "nex-1")
  exp <- list(vpus = c("01", "02"), n_flowpaths = 2L, n_divides = 2L, area_sqkm = 25)
  res <- suppressMessages(
    hf_check_merge_invariants(list(flowpaths = fp, divides = dv, nexus = nx),
      expected = exp, strict = FALSE))
  expect_true(res$ok)
})

test_that("hf_check_merge_invariants flags missing VPU, lost area, zeroed DA", {
  fp <- data.frame(flowpath_id = c("fp-1", "fp-2"), vpuid = c("01", "02"),
    total_dasqkm = c(0, 25),                       # one zeroed
    areasqkm = c(8, 20))            # fp-1 is a real catchment ->
  # DA==0 is a genuine violation
  # (not a catchment-less connector)
  dv <- data.frame(divide_id = c("cat-1", "cat-2"), vpuid = c("01", "02"),
    areasqkm = c(10, 15))
  nx <- data.frame(nexus_id = "nex-1")
  exp <- list(vpus = c("01", "02", "03"), n_flowpaths = 2L,        # VPU 03 missing
    n_divides = 2L, area_sqkm = 40)                     # area short
  res <- suppressMessages(
    hf_check_merge_invariants(list(flowpaths = fp, divides = dv, nexus = nx),
      expected = exp, strict = FALSE))
  expect_false(res$ok)
  expect_false(res$checks$all_vpus_present$ok)
  expect_false(res$checks$divide_area_conserved$ok)
  expect_false(res$checks$drainage_area_populated$ok)
})

test_that("hf_check_merge_invariants strict stops on failure", {
  fp <- data.frame(flowpath_id = "fp-1", vpuid = "01", total_dasqkm = 0,
    areasqkm = 1)                  # real catchment, DA==0 -> fail
  dv <- data.frame(divide_id = "cat-1", vpuid = "01", areasqkm = 1)
  expect_error(suppressMessages(
    hf_check_merge_invariants(list(flowpaths = fp, divides = dv, nexus = NULL),
      strict = TRUE)))
})
