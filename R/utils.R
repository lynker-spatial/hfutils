#' Check if a layer exists in a geopackage
#' @param gpkg path to .gpkg
#' @param name layer name
#' @return logical
#' @examples
#' \dontrun{
#' layer_exists("hydrofabric.gpkg", "divides")
#' }
#' @export
layer_exists <- function(gpkg, name) {
  file.exists(gpkg) && name %in% sf::st_layers(gpkg)$name
}

#' Get endpoint or startpoint of LINESTRING
#' @param x sf LINESTRING
#' @param position "start" or "end"
#' @return sf POINT
#' @examples
#' \dontrun{
#' fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' outlets <- get_node(sf::st_geometry(fl), position = "end")
#' }
#' @export

get_node <- function(x, position = "end") {
  if (position == "end") {
    lwgeom::st_endpoint(x)
  } else {
    lwgeom::st_startpoint(x)
  }
}

# network_is_dag (dplyr/ID-toID variant) removed -- consolidated into the single
# canonical hf_network_is_dag() in network_properties.R (id_col/toid_col API).

#' Add length and area measures to flowpaths/divides
#' @param flowpaths sf LINESTRING
#' @param divides sf POLYGON
#' @return named list of updated flowpaths and divides
#' @examples
#' \dontrun{
#' fps <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' divs <- sf::read_sf("hydrofabric.gpkg", "divides")
#' out <- add_measures(fps, divs)
#' out$flowpaths$lengthkm
#' }
#' @export
#'
add_measures <- function(flowpaths, divides) {
  flowpaths$lengthkm <- add_lengthkm(flowpaths)
  divides$areasqkm <- add_areasqkm(divides)
  flowpaths$areasqkm <- NULL
  div_tab <- sf::st_drop_geometry(divides)
  # Join incremental catchment area onto flowpaths. Prefer the explicit
  # divide->flowpath link (`flowpath_id` on divides), required by the current
  # schema where divide_id != flowpath_id (e.g. cat-* vs fp-*); joining on
  # divide_id there matches nothing and silently zeroes areasqkm (and any
  # downstream-accumulated total area). Fall back to the legacy 1:1 convention
  # (divide_id == flowpath_id) when divides carry no flowpath_id.
  if ("flowpath_id" %in% names(div_tab)) {
    flowpaths <- dplyr::left_join(
      flowpaths, dplyr::select(div_tab, flowpath_id, areasqkm),
      by = "flowpath_id")
  } else {
    flowpaths <- dplyr::left_join(
      flowpaths, dplyr::select(div_tab, divide_id, areasqkm),
      by = c("flowpath_id" = "divide_id"))
  }
  list(
    flowpaths = rename_geometry(flowpaths, "geometry"),
    divides   = rename_geometry(divides, "geometry")
  )
}

#' Rename geometry column of sf object
#' @param g sf object
#' @param name new geometry name. Default `"geometry"`.
#' @return sf object with renamed geometry
#' @examples
#' \dontrun{
#' fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' fl <- rename_geometry(fl, "geometry")
#' }
#' @export
rename_geometry <- function(g, name = "geometry") {
  current <- attr(g, "sf_column")
  if (identical(current, name)) return(g)
  names(g)[names(g) == current] <- name
  attr(g, "sf_column") <- name
  g
}

#' Node geometry from line endpoints
#'
#' Replaces the geometry column of an `sf` object with point geometries taken
#' from each feature's line endpoints (or start nodes). Thin wrapper around
#' [get_node()].
#'
#' @param x An `sf` object (typically LINESTRING/MULTILINESTRING).
#' @param position Character string, either `"end"` (default) or `"start"`,
#'   forwarded to [get_node()] to choose which node to extract.
#'
#' @return An `sf` object with geometry set to the requested node locations.
#'
#' @seealso [get_node()]
#'
#' @examples
#' \dontrun{
#' pts <- node_geometry(flow_sf, position = "end")
#' }
#'
#' @export
#' @importFrom sf st_set_geometry st_geometry

node_geometry <- function(x, position = "end") {
  sf::st_set_geometry(x, get_node(sf::st_geometry(x), position))
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
#'   CRS, `sf::st_length()` will compute ellipsoidal lengths when possible.
#'
#' @return A numeric vector of lengths in kilometers.
#'
#' @examples
#' \dontrun{
#' fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' fl$lengthkm <- add_lengthkm(fl)
#' }
#'
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_area

add_lengthkm <- function(x) {
  units::drop_units(units::set_units(sf::st_length(x), "km"))
}


#' Dissolve polygons by ID
#'
#' Dissolves a polygon layer by a grouping column, returning one feature per
#' group. Single-member groups short-circuit to their own geometry, so the cost
#' is paid only on groups that genuinely need unioning.
#'
#' @param poly An `sf` POLYGON/MULTIPOLYGON object with an attribute column
#'   used for grouping.
#' @param ID A string naming the column over which to union geometries.
#'
#' @return An `sf` polygon layer unioned by `ID` (column preserved).
#'
#' @section Notes:
#' If any resulting geometries are geometry collections, they are extracted to
#' POLYGON using `sf::st_collection_extract()`.
#'
#' The dissolve is **area-conserving and cannot introduce overlap**. Two earlier
#' behaviours are deliberately gone:
#'
#' * It no longer casts the result to `POLYGON` and keeps only the largest part
#'   per group. A group whose members are genuinely disjoint is a multipart
#'   catchment, not an error, and discarding the smaller parts silently deleted
#'   ground. Output is `MULTIPOLYGON`, which is lossless.
#' * It no longer routes geometry through `terra::makeValid()` before
#'   aggregating. That round-trip perturbed shared boundaries enough that
#'   neighbouring groups came out overlapping — on one CONUS VPU the summed area
#'   of the result exceeded the summed area of its inputs by 20 km2, all of it
#'   overlap between adjacent groups, which downstream cleanup then had to
#'   remove. Grouped `sf::st_union()` of already-valid inputs preserves the
#'   input tiling exactly.
#'
#' Invalid input geometry is repaired per group with `sf::st_make_valid()` only
#' when `sf::st_is_valid()` says it is needed, so valid inputs are untouched.
#'
#' @examples
#' \dontrun{
#' out <- union_polygons(counties_sf, "state_fips")
#' }
#'
#' @export
#' @importFrom sf st_as_sf st_collection_extract st_geometry_type st_area
#'   st_union st_geometry st_is_valid st_make_valid st_sfc st_crs st_cast
#' @importFrom dplyr select
#' @importFrom rlang sym !!

union_polygons <- function(poly, ID) {
  id_sym <- rlang::sym(ID)
  poly   <- dplyr::select(poly, !!id_sym)

  ids <- as.character(poly[[ID]])
  g   <- sf::st_geometry(poly)

  # Repair only what is actually broken; a valid input must pass through
  # untouched, because perturbing valid geometry is what created the overlap
  # this function used to hand downstream.
  bad <- !suppressWarnings(sf::st_is_valid(g))
  bad[is.na(bad)] <- TRUE
  if (any(bad)) g[bad] <- sf::st_make_valid(g[bad])

  keys <- unique(ids)
  idx  <- split(seq_along(ids), factor(ids, levels = keys))

  out <- lapply(idx, function(ii) {
    u <- if (length(ii) == 1L) g[[ii]] else sf::st_union(g[ii])[[1L]]
    u
  })

  poly <- sf::st_sf(
    stats::setNames(list(keys), ID),
    geometry = sf::st_sfc(out, crs = sf::st_crs(poly)),
    stringsAsFactors = FALSE)

  if (any(grepl("COLLECTION", sf::st_geometry_type(poly)))) {
    poly <- sf::st_collection_extract(poly, "POLYGON")
  }

  # Normalise to MULTIPOLYGON: lossless, and one row per ID regardless of how
  # many disjoint parts a group has.
  suppressWarnings(sf::st_cast(poly, "MULTIPOLYGON"))
}

#' Fast linestring union by ID
#'
#' Fast union/merge of lines by grouping column, using `terra`'s
#' `aggregate()` and returning an `sf` layer. Final conversion to
#' clean LINESTRINGs is delegated to an internal helper
#' `flowpaths_to_linestrings()`.
#'
#' @param lines An `sf` LINESTRING/MULTILINESTRING object with an attribute
#'   column used for grouping.
#' @param ID A string naming the column over which to union geometries.
#'
#' @return An `sf` lines layer unioned by `ID` (column preserved). Output is
#'   normalized to clean LINESTRINGs via [flowpaths_to_linestrings()].
#'
#' @seealso [flowpaths_to_linestrings()], [union_polygons()]
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

#' Convert MULTILINESTRINGS to LINESTRINGS
#' @param flowpaths a flowpath `sf` object
#' @return a `sf` object
#' @examples
#' \dontrun{
#' fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
#' fl <- flowpaths_to_linestrings(fl)
#' }
#' @export
#' @importFrom sf st_geometry_type st_geometry st_line_merge
#' @importFrom dplyr bind_rows

flowpaths_to_linestrings <- function(flowpaths) {
  bool <- (st_geometry_type(sf::st_geometry(flowpaths)) == "MULTILINESTRING")
  multis <- flowpaths[bool, ]
  if (nrow(multis) > 0) {
    merged <- st_line_merge(sf::st_geometry(multis))
    # Rebuild sfc to flush stale class after in-place element assignment
    sf::st_geometry(multis) <- sf::st_sfc(as.list(merged), crs = sf::st_crs(flowpaths))
  }
  singles <- flowpaths[!bool, ]
  out <- bind_rows(multis, singles)

  # After st_line_merge, any remaining MULTILINESTRING means non-contiguous members.
  # Warn but allow -- MULTILINESTRING is valid for flowpaths and topology is preserved.
  still_multi <- sf::st_geometry_type(out) == "MULTILINESTRING"
  if (any(still_multi))
    warning(sprintf(
      paste0("flowpaths_to_linestrings: %d group(s) are still MULTILINESTRING ",
             "after st_line_merge -- non-contiguous members (kept as-is)"),
      sum(still_multi)))

  out
}

# Quickly validate only the invalid pieces (faster than validating everything).
# Single shared definition for the package; used by clean_geometry() and friends.
#' @importFrom sf st_is_valid st_make_valid st_cast
#' @importFrom dplyr filter bind_rows
fast_validity_check <- function(x) {
  valid_flag <- sf::st_is_valid(x)
  if (all(valid_flag)) return(x)
  valid   <- dplyr::filter(x, valid_flag)
  invalid <- sf::st_make_valid(dplyr::filter(x, !valid_flag)) |>
    sf::st_cast("POLYGON")
  dplyr::bind_rows(valid, invalid)
}

#' Format numeric identifiers without scientific notation
#'
#' `as.character()` on a round-number double id (e.g. `22000000`) yields
#' `"2.2e+07"`, which then fails every downstream integer-string join and
#' silently drops the record from the network (lineage loss). This formats
#' numeric ids in plain-digit form, preserves a `".part"` split suffix verbatim
#' (`".10"` must not round-trip through `as.numeric()` to `".1"`), re-normalizes
#' an already-scientific string back to plain digits, and keeps `NA` as `NA`.
#'
#' @param v A vector of identifiers, numeric or character.
#' @return A character vector of plain-digit ids, with `NA` preserved.
#' @examples
#' hf_fmt_id(22000000)      # "22000000", not "2.2e+07"
#' hf_fmt_id("123.10")      # "123.10" (split suffix preserved)
#' @export
hf_fmt_id <- function(v) {
  if (is.numeric(v)) {
    out <- format(v, scientific = FALSE, trim = TRUE)
    out[is.na(v)] <- NA_character_
    return(out)
  }
  v <- as.character(v)
  sci <- grepl("[eE]", v) & !is.na(v)
  has  <- grepl("\\.", v) & !is.na(v) & !sci
  base <- ifelse(sci, v, ifelse(has, sub("\\..*$", "", v), v))
  suf  <- ifelse(has, sub("^[^.]*", "", v), "")
  bn   <- suppressWarnings(as.numeric(base))
  bf   <- format(bn, scientific = FALSE, trim = TRUE)
  out  <- paste0(bf, suf)
  out[is.na(bn) | is.na(v)] <- NA_character_
  out
}
