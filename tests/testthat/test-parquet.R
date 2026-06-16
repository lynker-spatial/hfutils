## GeoParquet round-trips (sfarrow-derived).

test_that("st_write_parquet / st_read_parquet round-trips geometry and CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  x <- nc_divides(10L)
  f <- tempfile(fileext = ".parquet")

  expect_invisible(suppressWarnings(st_write_parquet(x, f)))
  back <- st_read_parquet(f)

  expect_s3_class(back, "sf")
  expect_equal(nrow(back), nrow(x))
  expect_true(sf::st_crs(back) == sf::st_crs(x))   # semantic CRS equality
})

test_that("st_read_parquet errors on a file without geo metadata", {
  skip_if_not_installed("arrow")
  f <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(a = 1:3), f)
  expect_error(st_read_parquet(f), "No geometry metadata")
})

test_that("write_sf_dataset / read_sf_dataset round-trips a partitioned dataset", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  x <- nc_divides(10L)
  x$part <- rep(c("x", "y"), length.out = nrow(x))
  d <- tempfile()

  suppressWarnings(write_sf_dataset(x, d, partitioning = "part"))
  ds <- arrow::open_dataset(d)
  back <- read_sf_dataset(ds)

  expect_s3_class(back, "sf")
  expect_equal(nrow(back), nrow(x))
})

test_that("parquet writers/readers guard their arguments", {
  skip_if_not_installed("arrow")
  expect_error(st_write_parquet(data.frame(a = 1), tempfile()), "sf data format")
  expect_error(st_write_parquet(nc_divides(1L)), "Missing output file")
  expect_error(read_sf_dataset(), "Arrow dataset")
})

test_that("read_sf_dataset reads a grouped write and supports find_geom", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  x <- nc_divides(10L)
  x$part <- rep(c("x", "y"), length.out = nrow(x))
  d <- tempfile()
  suppressWarnings(write_sf_dataset(dplyr::group_by(x, part), d))   # grouped_df path

  # plain read of the partitioned dataset
  back <- read_sf_dataset(arrow::open_dataset(d))
  expect_s3_class(back, "sf")
  expect_equal(nrow(back), nrow(x))

  # find_geom re-attaches geometry to a query that did not select it
  q <- dplyr::select(arrow::open_dataset(d), part)
  back2 <- read_sf_dataset(q, find_geom = TRUE)
  expect_s3_class(back2, "sf")
  expect_equal(nrow(back2), nrow(x))
})

test_that("st_read_parquet honors col_select", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  x <- nc_divides(5L)
  f <- tempfile(fileext = ".parquet")
  suppressWarnings(st_write_parquet(x, f))

  back <- st_read_parquet(f, col_select = c("ID", "geometry"))
  expect_true(all(c("ID", "geometry") %in% names(back)))
  expect_s3_class(back, "sf")
})
