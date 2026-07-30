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

# This test previously asserted the opposite: that a disjoint group is reduced to
# its largest part, with a warning. That behaviour silently deleted ground, so the
# assertion has been inverted deliberately -- a group whose members are disjoint
# is a multipart catchment, not an error, and its area must survive.
test_that("union_polygons keeps every part of a disjoint group", {
  skip_if_not_installed("sf")
  nc <- nc_divides(50L)
  g <- rbind(sf::st_sf(grp = "a", geometry = sf::st_geometry(nc[1, ])),
    sf::st_sf(grp = "a", geometry = sf::st_geometry(nc[50, ])))
  before <- sum(as.numeric(sf::st_area(g)))
  out <- union_polygons(g, "grp")
  expect_equal(nrow(out), 1L)
  expect_true(all(sf::st_geometry_type(out) == "MULTIPOLYGON"))
  expect_equal(sum(as.numeric(sf::st_area(out))), before, tolerance = 1e-6)
})

test_that("union_polygons dissolves adjacent members without creating overlap", {
  skip_if_not_installed("sf")
  sq <- function(x0) sf::st_polygon(list(cbind(c(x0, x0+1, x0+1, x0, x0),
                                               c(0, 0, 1, 1, 0))))
  g <- sf::st_sf(grp = rep(c("a", "b"), each = 2),
                 geometry = sf::st_sfc(sq(0), sq(1), sq(2), sq(3), crs = 5070))
  out <- union_polygons(g, "grp")
  expect_equal(nrow(out), 2L)
  s <- sum(as.numeric(sf::st_area(out)))
  u <- as.numeric(sf::st_area(sf::st_union(sf::st_geometry(out))))
  # A tiling input must come out tiling: sum == union means zero overlap between
  # the dissolved groups. The terra round-trip this replaced did not hold here.
  expect_equal(s, u, tolerance = 1e-6)
  expect_equal(s, sum(as.numeric(sf::st_area(g))), tolerance = 1e-6)
})

test_that("layer_exists is FALSE for a missing file and TRUE for a present layer", {
  skip_if_not_installed("sf")
  expect_false(layer_exists(tempfile(), "anything"))
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(nc_divides(2L), f, "divides", quiet = TRUE)
  expect_true(layer_exists(f, "divides"))
  expect_false(layer_exists(f, "nope"))
})

test_that("hf_network_is_dag distinguishes acyclic from cyclic networks", {
  dag <- data.frame(flowpath_id = c("1", "2"), flowpath_toid = c("2", "0"))
  cyc <- data.frame(flowpath_id = c("1", "2"), flowpath_toid = c("2", "1"))
  expect_true(hf_network_is_dag(dag))
  expect_false(hf_network_is_dag(cyc))
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
