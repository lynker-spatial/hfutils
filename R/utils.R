#' Check if a layer exists in a geopackage
#' @param gpkg path to .gpkg
#' @param name layer name
#' @return logical
#' @export
layer_exists <- function(gpkg, name) {
  file.exists(gpkg) && name %in% sf::st_layers(gpkg)$name
}

#' Get endpoint or startpoint of LINESTRING
#' @param x sf LINESTRING
#' @param position "start" or "end"
#' @return sf POINT
#' @export

get_node <- function(x, position = "end") {
  if (position == "end") {
    lwgeom::st_endpoint(x)
  } else {
    lwgeom::st_startpoint(x)
  }
}

#' Test if flow network is acyclic
#' @param fl sf object with flowlines
#' @param ID character column name
#' @param toID character column name
#' @importFrom igraph  graph_from_data_frame is.dag
#' @return logical
#' @keywords internal
#'
network_is_dag <- function(fl, ID = "id", toID = "toid") {
  fl |>
    dplyr::select(dplyr::all_of(c(ID, toID))) |>
    sf::st_drop_geometry() |>
    igraph::graph_from_data_frame(directed = TRUE) |>
    igraph::is.dag()
}

#' Add length and area measures to flowpaths/divides
#' @param flowpaths sf LINESTRING
#' @param divides sf POLYGON
#' @return named list of updated flowpaths and divides
#' @export
#'
add_measures <- function(flowpaths, divides) {
  flowpaths$lengthkm <- add_lengthkm(flowpaths)
  divides$areasqkm <- add_areasqkm(divides)
  flowpaths$areasqkm <- NULL
  flowpaths <- dplyr::left_join(flowpaths,
                                dplyr::select(sf::st_drop_geometry(divides), divide_id, areasqkm),
                                by = c("flowpath_id" = "divide_id")
  )
  list(
    flowpaths = rename_geometry(flowpaths, "geometry"),
    divides   = rename_geometry(divides, "geometry")
  )
}

#' Rename geometry column of sf object
#' @param g sf object
#' @param name new geometry name
#' @return sf object with renamed geometry
#' @export
rename_geometry <- function(g, name) {
  current <- attr(g, "sf_column")
  names(g)[names(g) == current] <- name
  attr(g, "sf_column") <- name
  g
}

#' Node geometry from line endpoints
#'
#' Replaces the geometry column of an `sf` object with point geometries taken
#' from each feature's line endpoints (or start nodes). This is a thin wrapper
#' around an internal helper `.get_node()` that extracts the desired node from a
#' geometry vector.
#'
#' @param x An `sf` object (typically LINESTRING/MULTILINESTRING).
#' @param position Character string, either `"end"` (default) or `"start"`,
#'   forwarded to `.get_node()` to choose which node to extract.
#'
#' @return An `sf` object with geometry set to the requested node locations.
#'
#' @details
#' This function requires an internal helper `.get_node(geom, position)` that
#' returns an `sfc` of points given an `sfc` of line geometries and a position
#' of `"start"` or `"end"`. Make sure that helper exists in your package.
#'
#' @examples
#' \dontrun{
#' pts <- node_geometry(flow_sf, position = "end")
#' }
#'
#' @export
#' @importFrom sf st_set_geometry st_geometry

node_geometry <- function(x, position = "end") {
  sf::st_set_geometry(x, hfutils::get_node(sf::st_geometry(x), position))
}


#' Compute area in square kilometers (numeric)
#'
#' Safely compute polygon area in km^2 and return a plain numeric vector.
#'
#' @param x An `sf` object with polygonal geometry. If `x` is not in a projected
#'   CRS, `sf::st_area()` will compute ellipsoidal areas when possible.
#'
#' @return A numeric vector of areas in square kilometers.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' add_areasqkm(nc[1, ])
#' }
#'
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_area
add_areasqkm <- function(x) {
  units::drop_units(units::set_units(sf::st_area(x), "km^2"))
}

#' Compute length in kilometers (numeric)
#'
#' Safely compute linestring length in km and return a plain numeric vector.
#'
#' @param x An `sf` object with LINE* geometry. If `x` is not in a projected
#'   CRS, `sf::st_length()` will compute ellipsoidal areas when possible.
#'
#' @return A numeric vector of lengths in kilometers.
#'
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_area

add_lengthkm <- function(x) {
  units::drop_units(units::set_units(sf::st_length(x), "km"))
}


#' Fast polygon union by ID
#'
#' Significantly faster than `sf::st_union()`/`dplyr::summarise()` for
#' unioning large polygon datasets by a grouping column, by leveraging
#' {terra}’s `aggregate()` with a round-trip through `terra::vect()`.
#'
#' @param poly An `sf` POLYGON/MULTIPOLYGON object with an attribute column
#'   used for grouping.
#' @param ID A string naming the column over which to union geometries.
#'
#' @return An `sf` polygon layer unioned by `ID` (column preserved).
#'
#' @section Notes:
#' If any resulting geometries are geometry collections, they are extracted to POLYGON using `sf::st_collection_extract()`.
#'
#' @examples
#' \dontrun{
#' out <- union_polygons(counties_sf, "state_fips")
#' }
#'
#' @export
#' @importFrom terra vect makeValid aggregate
#' @importFrom sf st_as_sf st_collection_extract st_geometry_type
#' @importFrom dplyr select
#' @importFrom rlang sym !!

union_polygons <- function(poly, ID) {
  id_sym <- rlang::sym(ID)

  poly <- dplyr::select(poly, !!id_sym) |>
    terra::vect() |>
    terra::makeValid() |>
    terra::aggregate(by = ID) |>
    sf::st_as_sf() |>
    dplyr::select(!!id_sym)

  # Normalize geometry type if needed
  if (any(grepl("COLLECTION", sf::st_geometry_type(poly)))) {
    poly <- sf::st_collection_extract(poly, "POLYGON")
  }
  poly
}

#' Fast linestring union by ID
#'
#' Fast union/merge of lines by grouping column, using {terra}’s
#' `aggregate()` and returning an `sf` layer. Final conversion to
#' clean LINESTRINGs is delegated to an internal helper
#' `flowpaths_to_linestrings()`.
#'
#' @param lines An `sf` LINESTRING/MULTILINESTRING object with an attribute
#'   column used for grouping.
#' @param ID A string naming the column over which to union geometries.
#'
#' @return An `sf` lines layer unioned by `ID` (column preserved). The function
#'   calls `flowpaths_to_linestrings()` (package-internal) to ensure clean
#'   LINESTRING output.
#'
#' @details
#' Ensure your package provides `flowpaths_to_linestrings(x)` which
#' converts/normalizes any MULTILINESTRING results to LINESTRING where
#' appropriate and preserves attributes.
#'
#' @examples
#' \dontrun{
#' out <- union_linestrings(flow_sf, "group_id")
#' }
#'
#' @export
#' @importFrom terra vect aggregate
#' @importFrom sf st_as_sf st_make_valid
#' @importFrom dplyr select
#' @importFrom rlang sym !!

union_linestrings <- function(lines, ID) {
  id_sym <- rlang::sym(ID)

  lines |>
    terra::vect() |>
    terra::aggregate(by = ID) |>
    sf::st_as_sf() |>
    dplyr::select(!!id_sym) |>
    flowpaths_to_linestrings()
}

#' Convert MULITLINESTINGS to LINESTRINGS
#' @param flowpaths a flowpath `sf` object
#' @return a `sf` object
#' @export
#' @importFrom sf st_geometry_type st_geometry st_line_merge
#' @importFrom dplyr bind_rows

flowpaths_to_linestrings <- function(flowpaths) {
  bool <- (st_geometry_type(sf::st_geometry(flowpaths)) == "MULTILINESTRING")
  multis <- flowpaths[bool, ]
  if (nrow(multis) > 0) {
    sf::st_geometry(multis) <- st_line_merge(sf::st_geometry(multis))
  }
  singles <- flowpaths[!bool, ]

  bind_rows(multis, singles)
}

# quickly check and validate invalid geometries only
fast_validity_check <- function(x) {
  bool <- st_is_valid(x)
  valid <- filter(x, bool)
  invalid <- st_make_valid(filter(x, !bool)) %>%
    st_cast("POLYGON")

  return(bind_rows(valid, invalid))
}
