## Coverage for the shared staged invariant gate, hf_check_invariants().

test_that("refactored stage passes a clean network and is invisible", {
  refactored <- data.frame(
    flowpath_id   = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"),
    stringsAsFactors = FALSE
  )
  res <- expect_invisible(
    suppressMessages(hf_check_invariants("refactored", refactored = refactored))
  )
  expect_true(res$ok)
  expect_identical(res$stage, "refactored")
  expect_true(res$checks$refactored_unique$ok)
  expect_true(res$checks$refactored_topology$ok)
})

test_that("refactored stage catches duplicate ids", {
  dup <- data.frame(
    flowpath_id   = c("1", "1", "2"),
    flowpath_toid = c("2", "2", "0"),
    stringsAsFactors = FALSE
  )
  expect_error(suppressMessages(hf_check_invariants("refactored", refactored = dup)),
               "duplicate")
})

test_that("refactored stage catches a dangling downstream pointer", {
  broken <- data.frame(
    flowpath_id   = c("1", "2"),
    flowpath_toid = c("2", "999"),
    stringsAsFactors = FALSE
  )
  expect_error(suppressMessages(hf_check_invariants("refactored", refactored = broken)),
               "nonexistent")
})

test_that("strict = FALSE downgrades failures to messages and reports ok = FALSE", {
  broken <- data.frame(
    flowpath_id   = c("1", "2"),
    flowpath_toid = c("2", "999"),
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(
    hf_check_invariants("refactored", refactored = broken, strict = FALSE)
  )
  expect_false(res$ok)
  expect_false(res$checks$refactored_topology$ok)
})

test_that("ngen stage enforces fp-/cat- id prefixes", {
  good_fp <- data.frame(flowpath_id = c("fp-1", "fp-2"),
                        flowpath_toid = c("fp-2", "0"), stringsAsFactors = FALSE)
  res <- suppressMessages(hf_check_invariants("ngen", flowpaths = good_fp))
  expect_true(res$checks$ngen_fp_prefix$ok)

  bad_fp <- data.frame(flowpath_id = c("fp-1", "2"),
                       flowpath_toid = c("0", "0"), stringsAsFactors = FALSE)
  expect_error(suppressMessages(hf_check_invariants("ngen", flowpaths = bad_fp)),
               "do not start with 'fp-'")
})

test_that("aggregated stage catches duplicate flowpath_id", {
  fp <- data.frame(flowpath_id = c("1", "1"), stringsAsFactors = FALSE)
  expect_error(
    suppressMessages(hf_check_invariants("aggregated", flowpaths = fp)),
    "duplicate"
  )
})

test_that("unknown stage is rejected", {
  expect_error(hf_check_invariants("nope"), "should be one of")
})

test_that("refactored stage validates reconciled members against refactored ids", {
  refactored <- data.frame(
    flowpath_id   = c("1", "2", "3"),
    flowpath_toid = c("2", "3", "0"), stringsAsFactors = FALSE)
  reconciled <- data.frame(
    ID = c("10", "20"),
    member_flowpath_id = c("1,2", "3"), stringsAsFactors = FALSE)

  res <- suppressMessages(
    hf_check_invariants("refactored", refactored = refactored,
                        reconciled = reconciled))
  expect_true(res$ok)
  expect_true(res$checks$reconciled_has_members$ok)
  expect_true(res$checks$reconciled_members_exist$ok)
})

test_that("refactored stage flags a reconciled member that does not exist", {
  refactored <- data.frame(flowpath_id = "1", flowpath_toid = "0",
                           stringsAsFactors = FALSE)
  reconciled <- data.frame(ID = "10", member_flowpath_id = "1,999",
                           stringsAsFactors = FALSE)
  expect_error(
    suppressMessages(hf_check_invariants("refactored", refactored = refactored,
                                         reconciled = reconciled)),
    "not in refactored")
})

test_that("aggregated coverage is informational when there are no fp-divide pairs", {
  skip_if_not_installed("sf")
  fx <- agg_fixture()
  fx$flowpaths$divide_id <- NA_character_      # no pairs to check
  res <- suppressMessages(
    hf_check_invariants("aggregated", flowpaths = fx$flowpaths,
                        divides = fx$divides, coverage = TRUE))
  expect_true(res$ok)
  expect_identical(res$checks$flowpath_coverage$kind, "info")
})

test_that("reconciled stage passes a clean, well-oriented network", {
  skip_if_not_installed("sf")
  fx <- recon_fixture()
  res <- suppressMessages(
    hf_check_invariants("reconciled", reconciled = fx$reconciled,
                        divides = fx$divides))
  expect_true(res$ok)
  expect_true(res$checks$reconciled_flow_direction$ok)
  expect_true(res$checks$every_flowline_has_divide$ok)
  expect_true(res$checks$network_divides_have_area$ok)
  expect_true(res$checks$no_excessive_rings$ok)
  # landscape carry-through is informational
  expect_identical(res$checks$landscape_preserved$kind, "info")
})

test_that("reconciled stage flags a reversed flowline", {
  skip_if_not_installed("sf")
  fx <- recon_fixture(reverse_first = TRUE)
  expect_error(
    suppressMessages(hf_check_invariants("reconciled", reconciled = fx$reconciled,
                                         divides = fx$divides)),
    "reversed"
  )
})

test_that("aggregated coverage check passes when flowpaths lie in their divides", {
  skip_if_not_installed("sf")
  fx <- agg_fixture()
  res <- suppressMessages(
    hf_check_invariants("aggregated", flowpaths = fx$flowpaths,
                        divides = fx$divides, coverage = TRUE))
  expect_true(res$ok)
  expect_true(res$checks$flowpath_coverage$ok)
})

test_that("ngen stage validates prefixes, the fp->nexus->fp DAG, and coverage", {
  skip_if_not_installed("sf")
  fx <- ngen_fixture()
  res <- suppressMessages(
    hf_check_invariants("ngen", flowpaths = fx$flowpaths, divides = fx$divides,
                        nexus = fx$nexus, coverage = TRUE))
  expect_true(res$ok)
  expect_true(res$checks$ngen_fp_prefix$ok)
  expect_true(res$checks$ngen_cat_prefix$ok)
  expect_true(res$checks$network_is_dag$ok)
  expect_true(res$checks$flowpath_coverage$ok)
})

test_that("ngen stage detects a cycle in the fp->nexus->fp graph", {
  skip_if_not_installed("sf")
  fx <- ngen_fixture()
  # Make it cyclic: terminal nexus points back to the head flowpath
  fx$nexus$nexus_toid[fx$nexus$nexus_id == "nex-3"] <- "fp-1"
  expect_error(
    suppressMessages(hf_check_invariants("ngen", flowpaths = fx$flowpaths,
                                         nexus = fx$nexus)),
    "cycle"
  )
})
