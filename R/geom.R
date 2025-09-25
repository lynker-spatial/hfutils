#' Clean Catchment Geometry
#' @description Fix common geometry issues in DEM-derived catchments (disjoint
#' fragments, artifacts, invalid topologies) and optionally apply topology-
#' preserving simplification via mapshaper. If `flowlines` are provided, the
#' largest part per catchment is chosen preferentially where it touches the
#' corresponding flowline feature.
#'
#' @param catchments sf POLYGON/MULTIPOLYGON with a unique ID column.
#' @param flowlines optional sf LINESTRING/MULTILINESTRING for picking the
#'   "prime" part per catchment (largest piece that intersects the flowline).
#' @param fl_ID character. Unique identifier column in `flowlines` that matches `ID` in `catchments`.
#' @param ID character. Unique identifier column in `catchments`. Default: "ID".
#' @param keep numeric in (0,1]. Proportion of points to retain in simplification.
#'   If `NULL`, no simplification is performed. Default: `NULL`.
#' @param crs integer or object accepted by `sf::st_crs`. Should be a projected
#'   CRS suitable for area calculations. Default: 5070 (NAD83 / Conus Albers).
#' @param grid numeric. Snap-to-grid size (in target CRS units). Default: 0.0009.
#' @param gb integer. Heap GB for `mapshaper-xl` when `force = TRUE` and `gb > 8`.
#'   Default: 8.
#' @param force logical. Use system `mapshaper` / `mapshaper-xl` binaries directly
#'   for simplification (faster and more robust on large data). Default: FALSE.
#' @param sys logical or NULL. Use system mapshaper for rmapshaper calls. If NULL,
#'   auto-detect. Set env `TURN_OFF_SYS_MAPSHAPER=YUP` to force `FALSE`.
#'
#' @return sf with the same ID column and fixed geometry; includes `areasqkm`.
#' @export
#' @importFrom sf st_crs st_transform st_is_valid st_make_valid st_cast st_length st_intersects st_zm st_collection_extract st_geometry_type st_drop_geometry st_is_empty st_make_valid
#' @importFrom dplyr mutate select group_by ungroup filter left_join summarize distinct arrange slice slice_max bind_rows rename any_of add_count everything across
#' @importFrom rlang .data !! sym
#' @importFrom lwgeom st_snap_to_grid
#' @importFrom rmapshaper ms_dissolve ms_explode ms_simplify
#' @importFrom glue glue
#'
clean_geometry <- function(catchments,
                           flowlines = NULL,
                           fl_ID = NULL,
                           ID = "ID",
                           keep = NULL,
                           crs = 5070,
                           grid = 0.0009,
                           gb = 8,
                           force = FALSE,
                           sys = NULL) {

  stopifnot(inherits(catchments, "sf"))
  if (!ID %in% names(catchments)) stop("Column `", ID, "` not found in `catchments`.")

  if (!is.null(flowlines)) {
    if (!inherits(flowlines, "sf")) stop("`flowlines` must be an sf object.")
    if (is.null(fl_ID) || !fl_ID %in% names(flowlines)) {
      stop("`flowlines` provided but `fl_ID` is missing or not found.")
    }
  }

  # respect env override for system mapshaper
  if (identical(Sys.getenv("TURN_OFF_SYS_MAPSHAPER"), "YUP")) sys <- FALSE
  if (is.null(sys)) {
    sys <- FALSE
    # prefer rmapshaper’s own detection but don’t error if absent
    try(sys <- is.character(check_sys_mapshaper(verbose = FALSE)), silent = TRUE)
  }

  # normalize CRS (only transform if needed)
  target_crs <- sf::st_crs(crs)
  if (is.na(target_crs)) target_crs <- sf::st_crs(catchments)
  catchments <- sf::st_transform(catchments, target_crs)

  # grid snap then cast to single POLYGONs with validity cleanup
  catchments <- lwgeom::st_snap_to_grid(catchments, size = grid)

  polygons <- suppressWarnings({
    catchments |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_cast("POLYGON") |>
      fast_validity_check() |>
      dplyr::add_count(!!sym(ID), name = "part_count") %>%
      dplyr::mutate(areasqkm = add_areasqkm(.)) |>
      dplyr::mutate(tmpID = dplyr::row_number()) |>
      rename_geometry("geometry")
  })

  master_n <- nrow(catchments)

  # helper to ensure identical ID type before joins
  norm_id_type <- function(x, col) {
    if (is.numeric(x[[col]])) return(x)
    suppressWarnings({
      as_num <- suppressWarnings(as.numeric(x[[col]]))
      if (!anyNA(as_num)) x[[col]] <- as_num
    })
    x
  }

  # if all rows already 1 POLYGON per ID, we can short-circuit
  if (all(sf::st_geometry_type(polygons) == "POLYGON") &&
      nrow(polygons) == master_n &&
      all(polygons$part_count == 1)) {

    if (!is.null(keep)) {
      polygons <- tryCatch(
        simplify_process(polygons,
                         keep = keep,
                         sys = sys,
                         gb = gb,
                         force = force),
        error = function(e) {
          if (isTRUE(force)) {
            message("System mapshaper simplification failed: ", conditionMessage(e))
          } else {
            message("Simplification failed. Try `force = TRUE` to use system mapshaper.")
          }
          polygons
        }
      )
      polygons <- fast_validity_check(polygons)
    }

    return(dplyr::select(polygons, -any_of(c("part_count", "tmpID"))))
  }

  # --- Handle multi-part catchments (choose a main part, then optionally merge secondaries) ---

  # Separate polygons with >1 part per catchment
  extra_parts <- dplyr::filter(polygons, .data$part_count != 1)
  extra_parts <- dplyr::filter(extra_parts, !sf::st_is_empty(extra_parts))

  # dissolve by ID, then explode back to single parts
  extra_parts <- try({
    tmp <- rmapshaper::ms_dissolve(extra_parts, key = ID,
                                   copy_fields = names(extra_parts), sys = sys)
    rmapshaper::ms_explode(tmp, sys = sys)
  }, silent = TRUE)

  if (inherits(extra_parts, "try-error")) {
    # fall back to original pieces if dissolve fails
    extra_parts <- dplyr::filter(polygons, .data$part_count != 1)
  }

  extra_parts <- dplyr::filter(extra_parts, !sf::st_is_empty(extra_parts))
  extra_parts$tmpID <- seq_len(nrow(extra_parts))

  # mark prime parts (those that intersect their matching flowline) if flowlines provided
  if (!is.null(flowlines)) {
    u_ids <- unique(extra_parts[[ID]])
    fl <- dplyr::filter(flowlines, .data[[fl_ID]] %in% u_ids)
    # ensure same CRS & 2D
    fl <- sf::st_transform(sf::st_zm(fl), sf::st_crs(extra_parts))

    imap <- sf::st_intersects(extra_parts, fl)
    l <- lengths(imap)

    if (sum(l) > 0) {
      df <- data.frame(
        tmpID    = rep.int(extra_parts[["tmpID"]], times = l),
        uid      = rep.int(extra_parts[[ID]],       times = l),
        touch_id = fl[[fl_ID]][unlist(imap)]
      ) |>
        dplyr::group_by(.data$tmpID) |>
        dplyr::summarize(prime = any(.data$uid == .data$touch_id), .groups = "drop")
      extra_parts <- dplyr::left_join(extra_parts, df, by = "tmpID")
      extra_parts$prime[is.na(extra_parts$prime)] <- FALSE
    } else {
      extra_parts$prime <- FALSE
    }
  } else {
    extra_parts$prime <- FALSE
  }

  # recompute area and rank within each ID
  extra_parts <- extra_parts |>
    dplyr::mutate(areasqkm = add_areasqkm(extra_parts)) |>
    dplyr::arrange(dplyr::desc(.data$prime), dplyr::desc(.data$areasqkm)) |>
    dplyr::mutate(newID = dplyr::row_number())

  main_parts <- extra_parts |>
    dplyr::group_by(.data[[ID]]) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  small_parts <- dplyr::filter(extra_parts, !.data$newID %in% main_parts$newID)

  # sanity: 1 main part per exploded ID + existing single-part originals == original row count
  singles <- dplyr::filter(polygons, .data$part_count == 1)
  if ((nrow(main_parts) + nrow(singles)) != master_n) {
    stop("Geometry reconciliation failed: mismatched feature count.")
  }

  main_parts <- dplyr::bind_rows(main_parts, singles)

  # If there are remaining small parts, join them back to the appropriate main polygon using the
  # longest intersection (LINESTRING) heuristic; otherwise use main_parts as-is.
  if (nrow(small_parts) > 0) {
    small_parts <- tryCatch({
      tmp <- rmapshaper::ms_dissolve(small_parts, key = ID,
                                     copy_fields = names(small_parts), sys = sys)
      rmapshaper::ms_explode(tmp, sys = sys)
    }, error = function(e) NULL, warning = function(w) NULL)

    if (!is.null(small_parts) && nrow(small_parts) > 0) {
      small_parts <- small_parts |>
        dplyr::mutate(areasqkm = add_areasqkm(small_parts),
                      newID = dplyr::row_number()) |>
        dplyr::select(.data$newID, !!sym(ID))

      # intersect small parts with main parts; pick the longest boundary overlap
      ints <- tryCatch({
        suppressWarnings({
          sf::st_intersection(small_parts, sf::st_make_valid(main_parts)) |>
            sf::st_collection_extract("LINESTRING")
        })
      }, error = function(e) {
        sf::st_intersection(small_parts, sf::st_make_valid(main_parts)) |>
          sf::st_collection_extract("POINT")
      })

      ints <- ints |>
        dplyr::mutate(l = sf::st_length(ints)) |>
        dplyr::group_by(.data$newID) |>
        dplyr::slice_max(.data$l, with_ties = FALSE) |>
        dplyr::ungroup()

      glued <- dplyr::right_join(
        small_parts,
        dplyr::select(sf::st_drop_geometry(ints), !!sym(ID), .data$newID),
        by = "newID"
      ) |>
        dplyr::bind_rows(main_parts) |>
        dplyr::select(-any_of(c("areasqkm", "tmpID", "newID"))) |>
        dplyr::group_by(.data[[ID]]) |>
        dplyr::mutate(part_count = dplyr::n()) |>
        dplyr::ungroup() |>
        rename_geometry("geometry")

      # union polygons within each ID
      in_cat <- union_polygons(dplyr::filter(glued, .data$part_count > 1), ID) |>
        dplyr::bind_rows(dplyr::select(dplyr::filter(glued, .data$part_count == 1), !!sym(ID))) |>
        dplyr::mutate(tmpID = dplyr::row_number()) |>
        fast_validity_check()
    } else {
      in_cat <- fast_validity_check(main_parts)
    }
  } else {
    in_cat <- fast_validity_check(main_parts)
  }

  # optional simplification
  if (!is.null(keep)) {
    if (!(is.numeric(keep) && keep > 0 && keep <= 1)) stop("`keep` must be in (0,1].")
    in_cat <- tryCatch(
      simplify_process(in_cat, keep = keep, sys = sys, gb = gb, force = TRUE),
      error = function(e) {
        if (isTRUE(force)) {
          message("System mapshaper simplification failed: ", conditionMessage(e))
        } else {
          message("Simplification failed. Try `force = TRUE`.")
        }
        in_cat
      }
    )
    in_cat <- fast_validity_check(in_cat)
  }

  # rebuild attributes: preserve original non-geometry columns, (re)compute areas
  attrs <- catchments |>
    dplyr::select(-any_of(c("areasqkm"))) |>
    sf::st_drop_geometry() |>
    norm_id_type(ID)

  out <- in_cat |>
    dplyr::mutate(areasqkm = add_areasqkm(in_cat)) |>
    sf::st_transform(target_crs) |>
    norm_id_type(ID) |>
    dplyr::left_join(attrs, by = ID)

  out
}

# --- helpers ---

# Quickly validate only the invalid pieces (faster than validating everything)
fast_validity_check <- function(x) {
  valid_flag <- sf::st_is_valid(x)
  if (all(valid_flag)) return(x)
  valid   <- dplyr::filter(x, valid_flag)
  invalid <- sf::st_make_valid(dplyr::filter(x, !valid_flag)) |>
    sf::st_cast("POLYGON")
  dplyr::bind_rows(valid, invalid)
}

#' Rename geometry column safely
#' @noRd
rename_geometry <- function(x, name = "geometry") {
  if (attr(x, "sf_column") == name) return(x)
  sf::st_geometry(x) <- name
  x
}

#' Compute km2 area
#' @description Safely compute area in square kilometers (numeric vector)
#' @param x sf object
#' @return numeric
#' @export
#' @importFrom units set_units drop_units
add_areasqkm <- function(x) {
  units::drop_units(units::set_units(sf::st_area(x), "km2"))
}

#' Mapshaper simplification wrapper with robust system calls and validation
#' @importFrom yyjsonr write_geojson_file read_geojson_file
#' @keywords internal
simplify_process <- function(catchments, keep, sys, gb = 8, force = TRUE) {
  stopifnot(is.numeric(keep), keep > 0, keep <= 1)

  if (isTRUE(force)) {
    tmp  <- tempfile(fileext = ".geojson")
    tmp2 <- tempfile(fileext = ".geojson")
    crs0 <- sf::st_crs(catchments)

    yyjsonr::write_geojson_file(sf::st_transform(catchments, 4326), tmp)

    bin <- if (gb <= 8) "mapshaper" else "mapshaper-xl"
    cmd <- if (gb <= 8) {
      glue::glue("{shQuote(bin)} {shQuote(tmp)} -simplify {keep} keep-shapes -o {shQuote(tmp2)}")
    } else {
      glue::glue("{shQuote(bin)} {gb}gb {shQuote(tmp)} -simplify {keep} keep-shapes -o {shQuote(tmp2)}")
    }

    status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)
    if (!identical(status, 0L)) stop("mapshaper command failed (exit ", status, ").")

    cats <- suppressWarnings({
      yyjsonr::read_geojson_file(tmp2) |>
        sf::st_transform(crs0)
    })

    unlink(c(tmp, tmp2), force = TRUE)

  } else {
    cats <- rmapshaper::ms_simplify(
      catchments,
      keep = keep,
      keep_shapes = TRUE,
      sys = sys
    )
  }

  tt <- fast_validity_check(cats)
  # st_is_valid returns logical; ensure all TRUE
  if (!all(sf::st_is_valid(tt))) stop("Simplification produced invalid geometries.")
  tt
}
