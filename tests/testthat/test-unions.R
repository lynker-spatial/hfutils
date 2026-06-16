## Fast unions by ID and MULTILINESTRING normalization.

test_that("union_polygons merges polygons by group and preserves the key", {
  skip_if_not_installed("sf")
  g <- nc_divides(6L)
  g$grp <- c("a", "a", "a", "b", "b", "b")

  out <- union_polygons(g, "grp")
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$grp, c("a", "b"))
  expect_true(all(grepl("POLYGON", as.character(sf::st_geometry_type(out)))))
})

test_that("union_linestrings merges lines by group and returns line geometry", {
  skip_if_not_installed("sf")
  g <- nc_divides(6L)
  g$grp <- c("a", "a", "a", "b", "b", "b")
  lines <- suppressWarnings(sf::st_cast(g, "MULTILINESTRING"))

  out <- suppressWarnings(union_linestrings(lines, "grp"))
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$grp, c("a", "b"))
  expect_true(all(grepl("LINESTRING", as.character(sf::st_geometry_type(out)))))
})

test_that("flowpaths_to_linestrings collapses contiguous MULTILINESTRINGs", {
  skip_if_not_installed("sf")
  fp <- suppressWarnings(sf::st_cast(nc_divides(3L), "MULTILINESTRING"))
  out <- flowpaths_to_linestrings(fp)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 3L)
  expect_true(all(sf::st_geometry_type(out) == "LINESTRING"))
})
