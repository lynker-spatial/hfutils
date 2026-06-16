## Round-trip tests for GeoPackage dataset-version stamping.

make_gpkg <- function() {
  f <- tempfile(fileext = ".gpkg")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:2, ]
  sf::st_write(nc, f, "divides", quiet = TRUE)
  f
}

test_that("gpkg_set_version writes version + provenance that gpkg_get_version reads back", {
  skip_if_not_installed("sf")
  f <- make_gpkg()
  gpkg_set_version(f, "4.0.1",
                   provenance = list(software = "hydrofabric 0.1", vpuid = "09"))

  v <- gpkg_get_version(f)
  expect_equal(v$version, "4.0.1")
  expect_equal(v$int_version, 40001L)   # 4*10000 + 0*100 + 1
  expect_equal(v$provenance$software, "hydrofabric 0.1")
  expect_equal(v$provenance$vpuid, "09")
})

test_that("re-stamping is idempotent (no duplicate metadata rows)", {
  skip_if_not_installed("sf")
  f <- make_gpkg()
  gpkg_set_version(f, "1.2.3")
  gpkg_set_version(f, "4.0.1", provenance = list(a = 1))   # overwrite

  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  on.exit(DBI::dbDisconnect(con))
  n_md  <- DBI::dbGetQuery(con, "SELECT count(*) n FROM gpkg_metadata")$n
  n_ref <- DBI::dbGetQuery(con, "SELECT count(*) n FROM gpkg_metadata_reference")$n
  n_ext <- DBI::dbGetQuery(con,
    "SELECT count(*) n FROM gpkg_extensions WHERE extension_name='gpkg_metadata'")$n

  expect_equal(n_md, 3L)    # int + semver + provenance
  expect_equal(n_ref, 3L)
  expect_equal(n_ext, 2L)   # spec-strict: one row per metadata table

  # latest value wins
  expect_equal(gpkg_get_version(f)$version, "4.0.1")
})

test_that("gpkg_get_version returns NULL on an unstamped GeoPackage", {
  skip_if_not_installed("sf")
  expect_null(gpkg_get_version(make_gpkg()))
})
