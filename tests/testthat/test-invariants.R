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

test_that("ngen stage flags dropped/mis-mapped mainstem_id", {
  drop_fp <- data.frame(flowpath_id = paste0("fp-", 1:5),
    flowpath_toid = "0", mainstem_id = c(10, rep(NA_real_, 4)),  # 80% NULL
    stringsAsFactors = FALSE)
  res <- suppressMessages(hf_check_invariants("ngen", flowpaths = drop_fp,
    strict = FALSE))
  expect_false(res$checks$mainstem_id_populated$ok)

  full_fp <- transform(drop_fp, mainstem_id = 11:15)
  res2 <- suppressMessages(hf_check_invariants("ngen", flowpaths = full_fp,
    strict = FALSE))
  expect_true(res2$checks$mainstem_id_populated$ok)
})

test_that("ngen stage flags incomplete/duplicate hydroseq routing order", {
  na_fp <- data.frame(flowpath_id = paste0("fp-", 1:10), flowpath_toid = "0",
    hydroseq = c(1:5, rep(NA, 5)), stringsAsFactors = FALSE)        # 50% NA
  expect_false(suppressMessages(hf_check_invariants("ngen", flowpaths = na_fp,
    strict = FALSE))$checks$hydroseq_valid$ok)

  dup_fp <- data.frame(flowpath_id = paste0("fp-", 1:5), flowpath_toid = "0",
    hydroseq = c(1, 2, 2, 3, 4), stringsAsFactors = FALSE)          # duplicate
  expect_false(suppressMessages(hf_check_invariants("ngen", flowpaths = dup_fp,
    strict = FALSE))$checks$hydroseq_valid$ok)

  ok_fp <- data.frame(flowpath_id = paste0("fp-", 1:5), flowpath_toid = "0",
    hydroseq = 1:5, stringsAsFactors = FALSE)
  expect_true(suppressMessages(hf_check_invariants("ngen", flowpaths = ok_fp,
    strict = FALSE))$checks$hydroseq_valid$ok)
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

test_that("ngen slope_valid requires strictly positive slope, all-NA is info", {
  base <- data.frame(flowpath_id = c("fp-1", "fp-2", "fp-3"),
    flowpath_toid = "0", stringsAsFactors = FALSE)

  ok_fp <- transform(base, slope = c(0.01, 0.5, 0.002))
  expect_true(suppressMessages(hf_check_invariants("ngen", flowpaths = ok_fp,
    strict = FALSE))$checks$slope_valid$ok)

  # a zero, a negative, and an NA each make it fail
  bad_fp <- transform(base, slope = c(0.01, 0, -1))
  expect_false(suppressMessages(hf_check_invariants("ngen", flowpaths = bad_fp,
    strict = FALSE))$checks$slope_valid$ok)

  na_fp <- transform(base, slope = NA_real_)
  res <- suppressMessages(hf_check_invariants("ngen", flowpaths = na_fp,
    strict = FALSE))
  expect_identical(res$checks$slope_valid$kind, "info")

  # absent column -> no check emitted
  expect_null(suppressMessages(hf_check_invariants("ngen", flowpaths = base,
    strict = FALSE))$checks$slope_valid)
})

test_that("ngen So_valid checks flowpaths, flowlines, and the network table", {
  fp <- data.frame(flowpath_id = c("fp-1", "fp-2"), flowpath_toid = "0",
    So = c(0.003, 0.004), stringsAsFactors = FALSE)
  expect_true(suppressMessages(hf_check_invariants("ngen", flowpaths = fp,
    strict = FALSE))$checks$So_valid$ok)

  fp_bad <- transform(fp, So = c(0.003, 0))
  expect_false(suppressMessages(hf_check_invariants("ngen", flowpaths = fp_bad,
    strict = FALSE))$checks$So_valid$ok)

  # So carried only on the network attribute table is still validated
  fp_no_so <- fp[, c("flowpath_id", "flowpath_toid")]
  network  <- data.frame(flowpath_id = c("fp-1", "fp-2"), So = c(0.01, -1))
  expect_false(suppressMessages(hf_check_invariants("ngen", flowpaths = fp_no_so,
    network = network, strict = FALSE))$checks$So_valid$ok)
})

test_that("ngen lake_spatial_consistent flags reaches far from their lake", {
  skip_if_not_installed("sf")
  lakes <- sf::st_sf(
    lake_id  = "L1",
    geometry = sf::st_sfc(.sq(5000, 5000, r = 500), crs = 5070))

  # fp-1 sits inside the lake (distance 0); fp-2 is ~70 km away but stamped L1
  flowpaths <- sf::st_sf(
    flowpath_id   = c("fp-1", "fp-2"),
    flowpath_toid = "0",
    lake_id       = c("L1", "L1"),
    geometry = sf::st_sfc(.ls(5000, 5000, 5200, 5000),
      .ls(75000, 75000, 75200, 75000), crs = 5070))

  res <- suppressMessages(hf_check_invariants("ngen", flowpaths = flowpaths,
    lakes = lakes, strict = FALSE))
  expect_false(res$checks$lake_spatial_consistent$ok)

  # drop the far reach -> the remaining stamp is plausible
  ok <- suppressMessages(hf_check_invariants("ngen",
    flowpaths = flowpaths[1, ], lakes = lakes, strict = FALSE))
  expect_true(ok$checks$lake_spatial_consistent$ok)

  # no lakes supplied -> check is not emitted
  expect_null(suppressMessages(hf_check_invariants("ngen",
    flowpaths = flowpaths, strict = FALSE))$checks$lake_spatial_consistent)
})

test_that("hf_check_attr_bounds flags out-of-range and skips absent/zero-valid", {
  df <- data.frame(
    smcmax_mean     = c(0.4, 0.95, 0.2),   # 0.95 > 0.9 -> 1 bad
    bexp_mode       = c(5, 5, 20),         # 20 > 15 -> 1 bad
    glacier_percent = c(0, 0.5, 1),        # inclusive 0..1 -> all ok
    area_sqkm       = c(1, 2, 3),          # all ok
    not_a_bound     = c(9, 9, 9)           # no bound -> skipped
  )
  res <- suppressMessages(hf_check_attr_bounds(df, "divides", strict = FALSE))
  expect_false(res$ok)
  expect_false(res$checks[["smcmax_mean"]]$ok)
  expect_false(res$checks[["bexp_mode"]]$ok)
  expect_true(res$checks[["glacier_percent"]]$ok)
  expect_true(res$checks[["areasqkm"]]$ok)  # data col `area_sqkm` matched via alias, reported canonically
  expect_null(res$checks[["not_a_bound"]])
})

test_that("caveated bounds excluded by default; attr failures are soft under strict", {
  df <- data.frame(imperv_mean = c(0, 0.2, 0.5), smcmax_mean = c(0.5, 0.95, 0.5))
  res <- suppressMessages(hf_check_attr_bounds(df, "divides", strict = TRUE))
  expect_null(res$checks[["imperv_mean"]])      # caveated -> skipped
  expect_false(res$checks[["smcmax_mean"]]$ok)  # soft fail did not abort
})

test_that("hf_check_attr_bounds matches via aliases (schema name differences)", {
  # bound canonical name is `lengthkm`; data uses the source alias `length_km`
  df <- data.frame(length_km = c(0.5, -1, 2))   # -1 <= 0 -> 1 bad
  res <- suppressMessages(hf_check_attr_bounds(df, "flowpaths", strict = FALSE))
  expect_false(res$ok)
  expect_false(res$checks[["lengthkm"]]$ok)     # matched via alias, reported canonically
})
