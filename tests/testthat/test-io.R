## write_hydrofabric() / read_hydrofabric() round trip.

make_layers <- function() {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:3, ]
  list(divides   = nc,
       flowpaths = sf::st_cast(nc, "MULTILINESTRING"))
}

test_that("write_hydrofabric round-trips through read_hydrofabric", {
  skip_if_not_installed("sf")
  layers <- make_layers()
  f <- tempfile(fileext = ".gpkg")

  out <- write_hydrofabric(layers, f, verbose = FALSE)
  expect_true(file.exists(out))

  hf <- read_hydrofabric(f, verbose = FALSE)
  expect_named(hf, c("flowpaths", "divides"), ignore.order = TRUE)
  expect_s3_class(hf$divides, "sf")
  expect_equal(nrow(hf$divides), 3L)
})

test_that("write_hydrofabric appends a .gpkg extension when missing", {
  skip_if_not_installed("sf")
  layers <- make_layers()
  stem <- tempfile()                      # no extension
  out  <- write_hydrofabric(layers, stem, verbose = FALSE)
  expect_match(out, "\\.gpkg$")
  expect_true(file.exists(out))
})

test_that("read_hydrofabric can read a single realization and reproject", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  write_hydrofabric(make_layers(), f, verbose = FALSE)

  fp <- read_hydrofabric(f, realization = "flowpaths", crs = 5070, verbose = FALSE)
  expect_named(fp, "flowpaths")
  expect_equal(sf::st_crs(fp$flowpaths)$epsg, 5070L)
})

test_that("read_hydrofabric errors when nothing is available to read", {
  expect_error(read_hydrofabric(), "provide a GeoPackage")
})

test_that("write_hydrofabric writes mixed sf + attribute-table layers", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  net <- data.frame(id = 1:5, toid = c(2:5, 0))
  write_hydrofabric(list(divides = nc_divides(5L), network = net), f, verbose = FALSE)
  expect_setequal(sf::st_layers(f)$name, c("divides", "network"))
})

test_that("write_hydrofabric rejects an unnamed list and a non-data.frame table", {
  skip_if_not_installed("sf")
  expect_error(
    write_hydrofabric(list(nc_divides(2L)), tempfile(fileext = ".gpkg"), verbose = FALSE),
    "named")
  expect_error(
    write_hydrofabric(list(divides = nc_divides(2L), bad = 1:5),
                      tempfile(fileext = ".gpkg"), verbose = FALSE),
    "data.frame")
})

test_that("enforce_dm = TRUE without an hf_dm in scope errors clearly", {
  skip_if_not_installed("sf")
  expect_error(
    write_hydrofabric(list(divides = nc_divides(2L)), tempfile(fileext = ".gpkg"),
                      verbose = FALSE, enforce_dm = TRUE),
    "hf_dm")
})

test_that("read_hydrofabric accepts in-memory sf and harmonizes CRS", {
  skip_if_not_installed("sf")
  d  <- nc_divides(5L)
  fp <- suppressWarnings(sf::st_cast(d, "MULTILINESTRING"))
  out <- read_hydrofabric(divides = d, flowpaths = fp, crs = 4326, verbose = FALSE)
  expect_named(out, c("flowpaths", "divides"), ignore.order = TRUE)
  expect_equal(sf::st_crs(out$divides)$epsg, 4326L)
  expect_equal(sf::st_crs(out$flowpaths)$epsg, 4326L)
})

test_that("read_hydrofabric errors on ambiguous (multi-candidate) layers", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(3L), f, "flowpaths", quiet = TRUE)
  sf::st_write(nc_divides(3L), f, "flowlines", quiet = TRUE)
  expect_error(
    read_hydrofabric(f, realization = "flowpaths", verbose = FALSE),
    "[Mm]ultiple")
})
