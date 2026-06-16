## Small measure / node / geometry-name helpers.

test_that("add_areasqkm and add_lengthkm return plain numeric km units", {
  skip_if_not_installed("sf")
  d <- nc_divides(3L)
  a <- add_areasqkm(d)
  expect_type(a, "double")
  expect_length(a, 3L)
  expect_true(all(a > 0))

  l <- add_lengthkm(suppressWarnings(sf::st_cast(d, "MULTILINESTRING")))
  expect_type(l, "double")
  expect_true(all(l > 0))
})

test_that("rename_geometry renames the active geometry column and is a no-op when matched", {
  skip_if_not_installed("sf")
  d <- nc_divides(2L)
  r <- rename_geometry(d, "geom")
  expect_identical(attr(r, "sf_column"), "geom")
  expect_true("geom" %in% names(r))
  expect_identical(rename_geometry(r, "geom"), r)   # no-op
})

test_that("get_node and node_geometry extract line endpoints as points", {
  skip_if_not_installed("sf")
  ln <- suppressWarnings(sf::st_cast(nc_divides(3L), "MULTILINESTRING"))
  pts <- get_node(sf::st_geometry(ln), "end")
  expect_true(all(sf::st_geometry_type(pts) == "POINT"))

  ng <- node_geometry(ln, "start")
  expect_s3_class(ng, "sf")
  expect_true(all(sf::st_geometry_type(ng) == "POINT"))
})

test_that("union_polygons keeps the largest part for a disjoint group", {
  skip_if_not_installed("sf")
  nc <- nc_divides(50L)
  g <- rbind(sf::st_sf(grp = "a", geometry = sf::st_geometry(nc[1, ])),
             sf::st_sf(grp = "a", geometry = sf::st_geometry(nc[50, ])))
  expect_warning(out <- union_polygons(g, "grp"), "disjoint MULTIPOLYGON")
  expect_equal(nrow(out), 1L)
  expect_true(all(sf::st_geometry_type(out) == "POLYGON"))
})

test_that("layer_exists is FALSE for a missing file and TRUE for a present layer", {
  skip_if_not_installed("sf")
  expect_false(layer_exists(tempfile(), "anything"))
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(2L), f, "divides", quiet = TRUE)
  expect_true(layer_exists(f, "divides"))
  expect_false(layer_exists(f, "nope"))
})

test_that("network_is_dag distinguishes acyclic from cyclic networks", {
  skip_if_not_installed("sf")
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 5070)
  dag <- sf::st_sf(id = c("1", "2"), toid = c("2", "0"), geometry = pts)
  cyc <- sf::st_sf(id = c("1", "2"), toid = c("2", "1"), geometry = pts)
  expect_true(hfutils:::network_is_dag(dag))
  expect_false(hfutils:::network_is_dag(cyc))
})

test_that("add_measures joins divide areas onto flowpaths by id", {
  skip_if_not_installed("sf")
  d <- nc_divides(4L)
  divides   <- d
  divides$divide_id <- as.character(d$ID)
  flowpaths <- suppressWarnings(sf::st_cast(d, "MULTILINESTRING"))
  flowpaths$flowpath_id <- as.character(d$ID)

  out <- add_measures(flowpaths, divides)
  expect_named(out, c("flowpaths", "divides"))
  expect_true("lengthkm" %in% names(out$flowpaths))
  expect_true("areasqkm" %in% names(out$flowpaths))
  expect_true("areasqkm" %in% names(out$divides))
})
