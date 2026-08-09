## QGIS style injection into a GeoPackage's layer_styles table.

styled_gpkg <- function(env = parent.frame()) {
  f <- tempfile(fileext = ".gpkg")
  d <- nc_divides(4)
  sf::st_write(d, f, "divides", quiet = TRUE)
  sf::st_write(sf::st_cast(d, "MULTILINESTRING"), f, "flowpaths", quiet = TRUE)
  sf::st_write(sf::st_centroid(sf::st_geometry(d)) |> sf::st_sf(), f, "nexus",
    quiet = TRUE)
  # an attribute-only table, which has no geometry column to style
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbWriteTable(con, "network", data.frame(id = 1:3, toid = c(2L, 3L, 0L)))
  DBI::dbDisconnect(con)
  withr::defer(unlink(f), envir = env)
  f
}

read_styles <- function(gpkg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT * FROM layer_styles")
}

test_that("read_qml returns the file as one string", {
  qml <- read_qml(system.file("qml", "nexus.qml", package = "hfutils"))
  expect_length(qml, 1L)
  expect_true(startsWith(qml, "<!DOCTYPE qgis"))
  expect_true(grepl("</qgis>", qml, fixed = TRUE))
})

test_that("every shipped QML is well-formed and names a real layer style", {
  qml <- list.files(system.file("qml", package = "hfutils"), pattern = "[.]qml$",
    full.names = TRUE)
  expect_gt(length(qml), 0L)
  for (f in qml) {
    txt <- read_qml(f)
    expect_true(grepl("<qgis", txt, fixed = TRUE), info = basename(f))
    expect_true(grepl("renderer-v2", txt, fixed = TRUE), info = basename(f))
  }
})

test_that("a QML ships for every geometry-bearing hydrofabric layer", {
  have <- sub("[.]qml$", "",
    basename(list.files(system.file("qml", package = "hfutils"), pattern = "[.]qml$")))
  # the geometry-bearing tables of the hydrofabric data model; `network` and
  # `flowpath_attributes` are attribute-only and take no style
  expect_true(all(c("divides", "WB", "flowpaths", "flowlines",
    "hydrolocations", "lakes", "nexus") %in% have))
})

test_that("each QML declares the geometry type its layer actually has", {
  want <- c(divides = "2", WB = "2",            # polygon
            flowpaths = "1", flowlines = "1",   # line
            hydrolocations = "0", lakes = "0", nexus = "0")  # point
  for (lyr in names(want)) {
    f <- system.file("qml", paste0(lyr, ".qml"), package = "hfutils")
    got <- sub(".*<layerGeometryType>([0-9]+)</layerGeometryType>.*", "\\1",
      gsub("\n", "", read_qml(f)))
    expect_equal(got, want[[lyr]], info = lyr)
  }
})

test_that("append_style writes one layer_styles row per styled layer", {
  skip_if_not_installed("sf")
  f <- styled_gpkg()
  append_style(f, layer_names = c("divides", "flowpaths", "nexus"))

  s <- read_styles(f)
  expect_setequal(s$f_table_name, c("divides", "flowpaths", "nexus"))
  expect_true(all(nzchar(s$styleQML)))
  expect_true(all(grepl("<qgis", s$styleQML, fixed = TRUE)))
})

test_that("each layer gets its own QML, not another layer's", {
  skip_if_not_installed("sf")
  f <- styled_gpkg()
  # caller order deliberately not alphabetical: pairing a directory listing
  # against this order positionally swaps the styles
  append_style(f, layer_names = c("nexus", "flowpaths", "divides"))

  s <- read_styles(f)
  for (lyr in c("nexus", "flowpaths", "divides")) {
    got <- s$styleQML[s$f_table_name == lyr]
    want <- read_qml(system.file("qml", paste0(lyr, ".qml"), package = "hfutils"))
    expect_identical(got, want, info = lyr)
  }
})

test_that("attribute tables and unstyled layers are skipped, not errors", {
  skip_if_not_installed("sf")
  f <- styled_gpkg()
  # `network` has no geometry column; `not_a_layer` has no shipped QML
  expect_no_error(
    append_style(f, layer_names = c("divides", "network", "not_a_layer")))

  s <- read_styles(f)
  expect_equal(s$f_table_name, "divides")
})

test_that("append_style records the geometry column QGIS needs", {
  skip_if_not_installed("sf")
  f <- styled_gpkg()
  append_style(f, layer_names = "divides")

  s <- read_styles(f)
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expected <- DBI::dbGetQuery(con,
    "SELECT column_name FROM gpkg_geometry_columns WHERE table_name = 'divides'")[[1]]
  expect_equal(s$f_geometry_column, expected)
  expect_true(as.logical(s$useAsDefault))
})

test_that("re-stamping replaces rather than accumulates", {
  skip_if_not_installed("sf")
  f <- styled_gpkg()
  append_style(f, layer_names = c("divides", "flowpaths"))
  append_style(f, layer_names = c("divides", "flowpaths"))
  expect_equal(nrow(read_styles(f)), 2L)
})

test_that("a styled GeoPackage still resolves through as_ogr", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(4), f, "divides", quiet = TRUE)
  withr::defer(unlink(f))
  append_style(f, layer_names = "divides")

  # layer_styles must not count as a user layer, or the single-layer fabric
  # stops auto-resolving
  expect_s3_class(as_ogr(f), "tbl_OGRSQLConnection")
})

test_that("write_hydrofabric does not stamp styles by default", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  withr::defer(unlink(f))
  d <- nc_divides(3)
  write_hydrofabric(list(divides = d), f, verbose = FALSE)

  expect_false("layer_styles" %in% sf::st_layers(f)$name)
})

test_that("write_hydrofabric(styles = TRUE) stamps them", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  withr::defer(unlink(f))
  d <- nc_divides(3)
  write_hydrofabric(
    list(divides = d, flowpaths = sf::st_cast(d, "MULTILINESTRING")),
    f, verbose = FALSE, styles = TRUE)

  expect_true("layer_styles" %in% sf::st_layers(f)$name)
  s <- read_styles(f)
  expect_setequal(s$f_table_name, c("divides", "flowpaths"))
  # each layer still gets its own symbology through this path
  for (lyr in c("divides", "flowpaths")) {
    expect_identical(s$styleQML[s$f_table_name == lyr],
      read_qml(system.file("qml", paste0(lyr, ".qml"), package = "hfutils")),
      info = lyr)
  }
})

test_that("styles = TRUE tolerates layers with no shipped QML", {
  skip_if_not_installed("sf")
  f <- tempfile(fileext = ".gpkg")
  withr::defer(unlink(f))
  d <- nc_divides(3)
  # `network` is an attribute table; `widgets` has no QML at all
  expect_no_error(write_hydrofabric(
    list(divides = d, widgets = d,
         network = data.frame(id = 1:2, toid = c(2L, 0L))),
    f, verbose = FALSE, styles = TRUE))

  expect_equal(read_styles(f)$f_table_name, "divides")
  # the fabric itself is written regardless
  expect_true(all(c("divides", "widgets", "network") %in% sf::st_layers(f)$name))
})

test_that("append_style errors on a missing GeoPackage", {
  expect_error(append_style(tempfile(fileext = ".gpkg"), layer_names = "divides"),
    "does not exist")
})
