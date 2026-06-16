## Lazy GDAL/OGR-SQL backend behind as_ogr().

local_gpkg <- function(env = parent.frame()) {
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(10L), f, "divides", quiet = TRUE)
  withr::defer(unlink(f), envir = env)
  f
}

test_that("as_ogr returns a lazy dbplyr table for a named layer", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  tb <- as_ogr(f, "divides")
  expect_s3_class(tb, "tbl_OGRSQLConnection")
  expect_s3_class(tb, "tbl_lazy")
})

test_that("a single user layer is auto-selected when none is named", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  # gpkg_/rtree_/sqlite_ system tables are ignored, leaving just `divides`
  tb <- as_ogr(f)
  expect_s3_class(tb, "tbl_OGRSQLConnection")
})

test_that("lazy select + filter + collect materializes a data frame", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  d <- as_ogr(f, "divides") |>
    dplyr::select(ID, AREA) |>
    dplyr::filter(ID <= 3) |>
    dplyr::collect()

  expect_s3_class(d, "data.frame")
  expect_false(inherits(d, "sf"))   # geometry not selected
  expect_equal(nrow(d), 3L)
})

test_that("st_as_sf on a full layer returns an sf object", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  d <- sf::st_as_sf(as_ogr(f, "divides"))
  expect_s3_class(d, "sf")
  expect_equal(nrow(d), 10L)
})

test_that("a nonexistent layer errors", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  expect_error(as_ogr(f, "does_not_exist"), "not in gpkg")
})

test_that("a raw SQL query can be passed via query=", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  d <- as_ogr(f, query = "SELECT ID FROM divides") |> dplyr::collect()
  expect_s3_class(d, "data.frame")
  expect_true("ID" %in% names(d))
  expect_equal(nrow(d), 10L)
})

test_that("connecting with an empty DSN errors", {
  expect_error(DBI::dbConnect(OGRSQL(), ""), "DSN")
})

test_that("query takes precedence over a named layer (with a message)", {
  skip_if_not_installed("sf")
  f <- local_gpkg()
  expect_message(
    as_ogr(f, "divides", query = "SELECT ID FROM divides"),
    "ignored"
  )
})

test_that("as_ogr returns the connection when several layers exist and none is named", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(10L), f, "divides", quiet = TRUE)
  sf::st_write(nc_divides(10L), f, "flowpaths", quiet = TRUE)
  withr::defer(unlink(f))
  z <- suppressMessages(as_ogr(f))
  expect_s4_class(z, "OGRSQLConnection")
})
