## clean_geometry() — DEM-derived catchment repair.

test_that("single-part catchments pass through with areas, ids preserved", {
  skip_if_not_installed("sf")
  cats <- nc_divides(5L)
  out <- clean_geometry(cats, ID = "ID")

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 5L)
  expect_setequal(out$ID, cats$ID)
  expect_true("areasqkm" %in% names(out))
  expect_true(all(out$areasqkm > 0))
  expect_true(all(sf::st_is_valid(out)))
})

test_that("multi-part catchments resolve to one feature per id", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rmapshaper")
  cats <- nc_multipart()
  out <- suppressWarnings(clean_geometry(cats, ID = "ID"))

  expect_s3_class(out, "sf")
  expect_false(any(duplicated(out$ID)))
  expect_setequal(out$ID, c(1L, 2L))
  expect_true(all(sf::st_is_valid(out)))
})

test_that("flowlines drive prime-part selection without error", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rmapshaper")
  cats <- nc_multipart()
  # a flowline matching ID 1, lying on its largest (prime) part
  nc <- nc_divides(1L)
  fl  <- sf::st_sf(fl_ID = 1L,
                  geometry = suppressWarnings(sf::st_cast(sf::st_geometry(nc[1, ]),
                                                          "MULTILINESTRING")))
  out <- suppressWarnings(
    clean_geometry(cats, flowlines = fl, fl_ID = "fl_ID", ID = "ID")
  )
  expect_s3_class(out, "sf")
  expect_setequal(out$ID, c(1L, 2L))
  expect_true(all(sf::st_is_valid(out)))
})

test_that("flowlines without fl_ID are rejected", {
  skip_if_not_installed("sf")
  expect_error(
    clean_geometry(nc_divides(2L), flowlines = nc_divides(2L), ID = "ID"),
    "fl_ID"
  )
})

test_that("keep triggers topology-preserving simplification", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rmapshaper")
  out <- suppressWarnings(suppressMessages(
    clean_geometry(nc_divides(5L), ID = "ID", keep = 0.5)))
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 5L)
  expect_true(all(sf::st_is_valid(out)))
})

test_that("clean_geometry keeps the largest part of a disjoint id and never inflates area", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rmapshaper")
  m <- nc_multipart()
  parts <- suppressWarnings(sf::st_cast(sf::st_cast(m, "MULTIPOLYGON"), "POLYGON"))
  pa <- add_areasqkm(parts)
  largest_id1 <- max(pa[parts$ID == 1])

  out <- suppressWarnings(clean_geometry(m, ID = "ID"))

  # disjoint orphan is discarded -> the largest part is retained for id 1
  expect_equal(out$areasqkm[out$ID == 1], largest_id1, tolerance = 0.02)
  # repair never invents area
  expect_lte(sum(out$areasqkm), sum(pa) + 1)
})

test_that("a missing ID column is rejected", {
  skip_if_not_installed("sf")
  expect_error(clean_geometry(nc_divides(2L), ID = "not_a_column"),
               "not found")
})

test_that("invalid `keep` is rejected", {
  skip_if_not_installed("sf")
  # keep is validated on the multi-part branch; force a multi-part input
  expect_error(
    suppressWarnings(clean_geometry(nc_multipart(), ID = "ID", keep = 1.5)),
    "must be in"
  )
})
