## In-place GeoPackage mutations: gpkg_update_col(), gpkg_update_geom(),
## gpkg_exec(). These write destructively to a user's GeoPackage and drop the
## layer's triggers to do it, so the assertions below care as much about what
## the file looks like afterwards as about the values written.

mut_gpkg <- function(n = 5L, env = parent.frame()) {
  f <- tempfile(fileext = ".gpkg")
  d <- nc_divides(n)
  d$label <- paste0("orig-", d$ID)
  d$score <- as.numeric(d$ID) * 10
  sf::st_write(d, f, "divides", quiet = TRUE)
  withr::defer(unlink(f), envir = env)
  f
}

# Trigger names attached to a layer, sorted for stable comparison.
trigs_on <- function(gpkg, layer = "divides") {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sort(DBI::dbGetQuery(con,
    sprintf("SELECT name FROM sqlite_master WHERE type='trigger' AND tbl_name='%s'",
      layer))$name)
}

tables_in <- function(gpkg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbListTables(con)
}

# ---- gpkg_update_col -------------------------------------------------------

test_that("gpkg_update_col updates only the targeted rows", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()

  gpkg_update_col(f, "divides", "ID", c(2, 4), "label", c("new-2", "new-4"))

  d <- sf::read_sf(f, "divides")
  d <- d[order(d$ID), ]
  expect_equal(d$label,
    c("orig-1", "new-2", "orig-3", "new-4", "orig-5"))
})

test_that("gpkg_update_col leaves the layer readable and its triggers intact", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  before <- trigs_on(f)
  expect_gt(length(before), 0L)   # guard: the fixture must actually have triggers

  gpkg_update_col(f, "divides", "ID", 1, "label", "x")

  expect_equal(trigs_on(f), before)
  # still a valid, readable spatial layer with its geometry untouched
  d <- sf::read_sf(f, "divides")
  expect_s3_class(d, "sf")
  expect_equal(nrow(d), 5L)
  expect_false(any(sf::st_is_empty(d)))
  expect_true("divides" %in% sf::st_layers(f)$name)
})

test_that("gpkg_update_col writes numeric values that read back as numeric", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  # values go through a character staging table, so column affinity is what
  # keeps a numeric column numeric
  gpkg_update_col(f, "divides", "ID", c(1, 2), "score", c(1.5, 2.5))

  d <- sf::read_sf(f, "divides")
  d <- d[order(d$ID), ]
  expect_type(d$score, "double")
  expect_equal(d$score[1:2], c(1.5, 2.5))
  expect_equal(d$score[3:5], c(30, 40, 50))
})

test_that("gpkg_update_col rolls back cleanly on a bad column", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  before <- trigs_on(f)
  labels_before <- sf::read_sf(f, "divides")$label

  expect_error(
    gpkg_update_col(f, "divides", "ID", 1, "no_such_column", "x"))

  # the transaction rolled back, so both the data and the dropped triggers
  # must be back exactly as they were
  expect_equal(trigs_on(f), before)
  expect_equal(sf::read_sf(f, "divides")$label, labels_before)
})

# ---- gpkg_update_geom ------------------------------------------------------

test_that("gpkg_update_geom swaps geometry for the targeted rows only", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  orig <- sf::read_sf(f, "divides")
  orig <- orig[order(orig$ID), ]

  # shift row 2 well away from where it started
  changed <- orig[orig$ID == 2, ]
  sf::st_geometry(changed) <- sf::st_geometry(changed) + c(100000, 100000)
  sf::st_crs(changed) <- sf::st_crs(orig)

  gpkg_update_geom(f, "divides", "ID", changed)

  after <- sf::read_sf(f, "divides")
  after <- after[order(after$ID), ]
  expect_equal(nrow(after), 5L)

  moved <- !sf::st_equals(after, orig, sparse = FALSE)[cbind(1:5, 1:5)]
  expect_equal(which(moved), 2L)
})

test_that("gpkg_update_geom removes its temporary layer completely", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  tables_before <- tables_in(f)

  changed <- sf::read_sf(f, "divides")
  changed <- changed[changed$ID == 1, ]
  sf::st_geometry(changed) <- sf::st_geometry(changed) + c(1000, 1000)
  sf::st_crs(changed) <- sf::st_crs(sf::read_sf(f, "divides"))

  gpkg_update_geom(f, "divides", "ID", changed)

  # no geom_upd_* table, and no orphan rows left in the gpkg catalog tables
  expect_false(any(grepl("^geom_upd_", tables_in(f))))
  expect_false(any(grepl("^geom_upd_", sf::st_layers(f)$name)))

  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con,
      "SELECT count(*) n FROM gpkg_contents WHERE table_name LIKE 'geom_upd_%'")$n, 0L)
  expect_equal(
    DBI::dbGetQuery(con,
      "SELECT count(*) n FROM gpkg_geometry_columns WHERE table_name LIKE 'geom_upd_%'")$n, 0L)
  # the only table added or removed overall is none
  expect_setequal(setdiff(tables_in(f), tables_before), character(0))
})

test_that("gpkg_update_geom restores the layer triggers", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  before <- trigs_on(f)

  changed <- sf::read_sf(f, "divides")
  changed <- changed[changed$ID == 3, ]
  sf::st_geometry(changed) <- sf::st_geometry(changed) + c(500, 500)
  sf::st_crs(changed) <- sf::st_crs(sf::read_sf(f, "divides"))

  gpkg_update_geom(f, "divides", "ID", changed)
  expect_equal(trigs_on(f), before)
})

# ---- gpkg_exec -------------------------------------------------------------

test_that("gpkg_exec runs statements in order in one transaction", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()

  # triggers must be dropped for any UPDATE on a spatial layer: the R-tree
  # triggers call ST_IsEmpty, which RSQLite does not provide
  gpkg_exec(f,
    list(drop_triggers_for = "divides"),
    "UPDATE divides SET label = 'a' WHERE ID = 1",
    "UPDATE divides SET label = 'b' WHERE ID = 1",
    "UPDATE divides SET label = 'c' WHERE ID = 2")

  d <- sf::read_sf(f, "divides")
  expect_equal(d$label[d$ID == 1], "b")   # last write wins, so order held
  expect_equal(d$label[d$ID == 2], "c")
})

test_that("gpkg_exec stages a data frame as a temporary table", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()

  gpkg_exec(f,
    list(table = "._patch",
      df = data.frame(k = c("1", "3"), v = c("p1", "p3"),
        stringsAsFactors = FALSE)),
    list(drop_triggers_for = "divides"),
    paste('UPDATE divides SET label = (SELECT v FROM "._patch"',
      "WHERE CAST(k AS TEXT) = CAST(divides.ID AS TEXT))",
      'WHERE CAST(ID AS TEXT) IN (SELECT k FROM "._patch")'))

  d <- sf::read_sf(f, "divides")
  d <- d[order(d$ID), ]
  expect_equal(d$label, c("p1", "orig-2", "p3", "orig-4", "orig-5"))
  # the staging table was temporary, so it must not persist in the file
  expect_false("._patch" %in% tables_in(f))
})

test_that("gpkg_exec drops and restores triggers on request", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  before <- trigs_on(f)

  gpkg_exec(f,
    list(drop_triggers_for = "divides"),
    "UPDATE divides SET label = 'z' WHERE ID = 1")

  expect_equal(trigs_on(f), before)
  expect_equal(sf::read_sf(f, "divides")$label[
    sf::read_sf(f, "divides")$ID == 1], "z")
})

test_that("gpkg_exec rolls back every statement when one fails", {
  skip_if_not_installed("sf")
  f <- mut_gpkg()
  before <- trigs_on(f)
  labels_before <- sf::read_sf(f, "divides")$label

  expect_error(
    gpkg_exec(f,
      list(drop_triggers_for = "divides"),
      "UPDATE divides SET label = 'committed?' WHERE ID = 1",
      "UPDATE divides SET label = 'x' WHERE no_such_column = 1"))

  # the earlier successful UPDATE must not have survived, and the triggers
  # dropped before it must be back
  expect_equal(sf::read_sf(f, "divides")$label, labels_before)
  expect_equal(trigs_on(f), before)
})
