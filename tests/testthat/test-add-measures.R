test_that("add_measures joins divide area by flowpath_id when divide_id != flowpath_id", {
  skip_if_not_installed("sf")
  fp <- sf::st_sf(
    flowpath_id = c("fp-1", "fp-2"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1000, 1000), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1000, 1000, 2000, 2000), ncol = 2, byrow = TRUE))),
    crs = 5070)
  # cat-* divides carry an explicit flowpath_id link; divide_id != flowpath_id
  div <- sf::st_sf(
    divide_id   = c("cat-1", "cat-2"),
    flowpath_id = c("fp-1", "fp-2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1000, 0, 1000, 1000, 0, 1000, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 0, 2000, 0, 2000, 2000, 0, 2000, 0, 0), ncol = 2, byrow = TRUE)))),
    crs = 5070)
  res <- add_measures(fp, div)
  expect_true(all(res$flowpaths$areasqkm > 0))          # not zeroed by a bad join
  expect_equal(res$flowpaths$areasqkm[2] / res$flowpaths$areasqkm[1], 4, tolerance = 1e-6)
})

test_that("add_measures falls back to legacy divide_id == flowpath_id", {
  skip_if_not_installed("sf")
  fp <- sf::st_sf(
    flowpath_id = c("1", "2"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1000, 1000), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1000, 1000, 2000, 2000), ncol = 2, byrow = TRUE))),
    crs = 5070)
  div <- sf::st_sf( # no flowpath_id column -> legacy join on divide_id
    divide_id = c("1", "2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1000, 0, 1000, 1000, 0, 1000, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 0, 2000, 0, 2000, 2000, 0, 2000, 0, 0), ncol = 2, byrow = TRUE)))),
    crs = 5070)
  res <- add_measures(fp, div)
  expect_true(all(res$flowpaths$areasqkm > 0))
})
