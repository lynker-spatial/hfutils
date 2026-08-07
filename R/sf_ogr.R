#' @include OGRSQLConnection.R
#' @include OGRSQLDriver.R
NULL

# Data-source plumbing, not user layers. Prefixes are anchored so a real layer
# containing one of these fragments (flowpaths_gpkg_v2) survives; the QGIS
# tables are fixed names, so they match exactly.
#
#   gpkg_*         GeoPackage spec tables (incl. GDAL's gpkg_ogr_contents)
#   rtree_*        spatial index
#   sqlite_*       SQLite internals (sqlite_sequence, sqlite_stat1, ...)
#   layer_styles   QGIS style saved into the GeoPackage
#   qgis_projects  QGIS project saved into the GeoPackage
#
# Shared by the generic and both methods so the defaults cannot drift.
.hf_ignore_lyrs <- "^gpkg_|^rtree_|^sqlite_|^layer_styles$|^qgis_projects$"

#' OGRSQL
#' OGRSQL driver, use to [dbConnect()] to a data source readable by sf
#' @examples
#' \dontrun{
#' con <- dbConnect(OGRSQL(), "hydrofabric.gpkg")
#' as_ogr(con, "flowpaths")
#' }
#' @export

OGRSQL <- function() {
  new("OGRSQLDriver")
}

#' dbConnect
#'
#' dbConnect for sources that can be read by package sf
#'
#' The 'OGRSQL' available is documented with GDAL: https://gdal.org/user/ogr_sql_dialect.html
#' @param drv OGRSQLDriver created by \code{OGRSQL()}
#' @param DSN  data source name
#' @param readonly open in readonly mode (`TRUE` is the only option)
#' @param ... ignored
#' @export

setMethod("dbConnect", "OGRSQLDriver",
  function(drv, DSN = "", readonly = TRUE, ...) {
    if (nchar(DSN) < 1) stop("DSN must be a valid data source name (file, connection string, url, ...)")
    new("OGRSQLConnection", DSN = DSN,  readonly = readonly, ...)
  })

#' @rdname OGRSQLConnection-class
#' @export

setMethod("dbDisconnect", "OGRSQLConnection", function(conn, ...) {
  conn@DSN <- ""
  conn
})

#' Delayed read for vector resources
#'
#' A lazy data frame for GDAL vector data sources. as_ogr is DBI compatible and designed to work with dplyr.
#'
#' The output of `as_ogr()` is a 'tbl_OGRSQLConnection` that extends `tbl_dbi` and
#' may be used with functions and workflows in the normal DBI way, see [OGRSQL()] for
#' the as_ogr DBI support.
#'
#' To obtain an in memory data frame use an explicit `collect()` or `st_as_sf()`.
#' A call to `collect()` is triggered by `st_as_sf()` and will add the sf class
#' to the output.
#'
#' @inheritParams sf::read_sf
#' @param x the data source (file path, url, or database connection)
#' @param query SQL query to pass in directly
#' @param ignore_lyrs Regular expression matching tables that are data-source
#'   plumbing rather than user layers, excluded when `layer` is not given. The
#'   default drops GeoPackage spec tables (`^gpkg_`), spatial indexes
#'   (`^rtree_`), SQLite internals (`^sqlite_`), and the two tables QGIS writes
#'   into a GeoPackage when you save a style or a project to it
#'   (`layer_styles`, `qgis_projects`). Patterns are anchored, so a layer whose
#'   name merely contains one of these is not dropped.
#' @return a 'tbl_OGRSQLConnection'
#' @examples
#' \dontrun{
#' library(dplyr)
#' tbl <- as_ogr("hydrofabric.gpkg", "flowpaths")
#' tbl |> filter(order >= 4) |> st_as_sf()
#' }
#' @export

as_ogr <- function(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs) {
  UseMethod("as_ogr")
}

#' @name as_ogr
#' @export
as_ogr.character <- function(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs) {
  db <- dbConnect(OGRSQL(), x)
  as_ogr(db, layer, ..., query = query, ignore_lyrs = ignore_lyrs)
}

#' @name as_ogr
#' @export
as_ogr.OGRSQLConnection <- function(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs) {

  if (!is.na(query)) {
    if (!missing(layer)) message("'layer' argument ignored, using 'query'")
    return(dplyr::tbl(x, dbplyr::sql(query)))
  }

  if (missing(layer)) {
    tbls <- dbListTables(x)
    tbls <- tbls[!grepl(ignore_lyrs, tbls)]

    if (length(tbls) == 0L) {
      cli::cli_abort("No readable layers found in the data source.")
    } else if (length(tbls) == 1L) {
      layer <- tbls
    } else {
      cli::cli_abort(c(
        "Multiple layers found; please specify {.arg layer} explicitly:",
        ">" = paste(tbls, collapse = ", ")
      ))
    }
  }

  if (layer %in% dbListTables(x)) {
    x <- tbl(x, layer)
  } else {
    cli::cli_abort("{.val {layer}} not in gpkg.")
  }

  x

}

#' Force collection of a OGR query
#' Convert as_ogr to a data frame or sf object
#' @param x output of [as_ogr()]
#' @param ... passed to [collect()]
#' @name st_as_sf
#' @return a data frame from `collect()`, sf data frame from `st_as_sf()` (only if it contains an `sfc` geometry column)
#' @examples
#' \dontrun{
#' q <- as_ogr("hydrofabric.gpkg", "flowpaths")
#' df <- collect(q)        # plain data frame
#' sf <- st_as_sf(q)       # sf object (collect() triggered internally)
#' }
#' @importFrom sf st_as_sf
#' @importFrom dplyr collect
#' @export
#' @export st_as_sf
#' @export collect
#' @aliases collect

st_as_sf.tbl_OGRSQLConnection <- function(x, ...) {

  d <- collect(x, ...)

  if (any(c("geom", "geometry") %in% colnames(d))) {
    st_as_sf(d)
  } else {
    d
  }

}
