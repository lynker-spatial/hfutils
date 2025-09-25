#' Read Hydrofabric Layers from a GeoPackage (or accept in-memory `sf` objects)
#'
#' @description
#' Convenience reader for a Hydrofabric GeoPackage that returns a named list of
#' `sf` objects: `list(divides = ..., flowpaths = ...)`. You may provide a
#' GeoPackage path and let the function auto-detect layer names, *or* pass
#' in-memory `sf` objects directly. Optionally transforms to a target CRS and
#' harmonizes CRS across returned layers.
#'
#' @param gpkg Character path to a GeoPackage. If `NULL`, the function will only
#'   use the `divides` and/or `flowpaths` arguments (if provided). Default: `NULL`.
#' @param divides Either `NULL`, a character layer name within `gpkg`, or an
#'   in-memory `sf` object representing divides/catchments.
#' @param flowpaths Either `NULL`, a character layer name within `gpkg`, or an
#'   in-memory `sf` object representing flowpaths/flowlines.
#' @param realization One of `"all"`, `"divides"`, or `"flowpaths"`. Controls
#'   which layers to auto-discover/read from `gpkg` when corresponding arguments
#'   are `NULL`. Default: `"all"`.
#' @param crs Optional CRS to transform outputs to (e.g., EPSG code like `5070`,
#'   an `sf::crs` object, or any input accepted by `sf::st_crs()`).
#' @param verbose Logical; print progress messages. Default uses the environment
#'   variable `hydrofab_verbose` (anything other than literal `"false"` is treated
#'   as verbose).
#'
#' @return A named list with up to two elements:
#'   \itemize{
#'     \item `divides`: an `sf` object (if available)
#'     \item `flowpaths`: an `sf` object (if available)
#'   }
#'
#' @details
#' - If `divides`/`flowpaths` are provided as in-memory `sf` objects, these are
#'   used as-is (subject to optional CRS transformation).
#' - If `gpkg` is provided and a corresponding layer argument is:
#'   \itemize{
#'     \item `NULL`: the function tries to auto-detect the layer name.
#'     \item a character: that name is used (and validated).
#'   }
#' - Auto-discovery looks for:
#'   \itemize{
#'     \item Flowpaths: names matching `flowpath|flowline`, excluding
#'       `attributes|edge_list`.
#'     \item Divides: names matching `divide|catchment`, excluding `network`.
#'   }
#'   If multiple candidates are found, an error is thrown with the candidates listed.
#'
#' CRS behavior:
#' - If `crs` is provided, all returned layers are transformed to that CRS.
#' - Otherwise, if two layers are returned with different CRSs, the second is
#'   transformed to the CRS of the first.
#'
#' @examples
#' \dontrun{
#' # Auto-detect layers from a HydroFabric GPKG and return both
#' x <- read_hydrofabric("path/to/hydrofabric.gpkg")
#'
#' # Read only flowpaths (auto-detected) and transform to EPSG:5070
#' x <- read_hydrofabric("path/to/hydrofabric.gpkg", realization = "flowpaths", crs = 5070)
#'
#' # Supply in-memory sf layers (no gpkg), harmonize to EPSG:3857
#' x <- read_hydrofabric(divides = my_divides_sf, flowpaths = my_flow_sf, crs = 3857)
#' }
#'
#' @importFrom sf st_layers read_sf st_crs st_transform
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @export

read_hydrofabric <- function(gpkg = NULL,
                             divides = NULL,
                             flowpaths = NULL,
                             realization = c("all", "divides", "flowpaths"),
                             crs = NULL,
                             verbose = Sys.getenv("hydrofab_verbose") != "false") {

  realization <- match.arg(realization)

  # ---- helpers --------------------------------------------------------------
  .is_sf <- function(x) inherits(x, "sf")

  .layer_names <- function(dsn) {
    tryCatch(sf::st_layers(dsn)$name,
             error = function(e) cli::cli_abort(c(
               "Failed to list layers from {.path {dsn}}.",
               "x" = conditionMessage(e)
             )))
  }

  .layer_exists <- function(dsn, layer) {
    layer %in% .layer_names(dsn)
  }

  .choose_single <- function(cands, what) {
    if (length(cands) == 0) return(NULL)
    if (length(cands) > 1) {
      cli::cli_abort(c(
        "Multiple {what} layer candidates found. Please specify one explicitly:",
        ">" = paste(cands, collapse = ", ")
      ))
    }
    cands
  }

  .autodetect_flowpaths <- function(dsn) {
    nms <- .layer_names(dsn)
    cands <- grep("flowpath|flowline", nms, value = TRUE, ignore.case = TRUE)
    cands <- cands[!grepl("attributes|edge_list", cands, ignore.case = TRUE)]
    .choose_single(cands, "flowpath")
  }

  .autodetect_divides <- function(dsn) {
    nms <- .layer_names(dsn)
    cands <- grep("divide|catchment", nms, value = TRUE, ignore.case = TRUE)
    cands <- cands[!grepl("network", cands, ignore.case = TRUE)]
    .choose_single(cands, "divide")
  }

  .read_layer <- function(dsn, layer, label) {
    if (!.layer_exists(dsn, layer)) {
      cli::cli_abort("{label} layer '{layer}' not found in {.path {dsn}}.")
    }
    if (isTRUE(verbose)) cli::cli_alert_info("Reading {label} from: {layer}")
    sf::read_sf(dsn = dsn, layer = layer)
  }

  .harmonize_crs_pair <- function(lst) {
    if (!is.null(lst$divides) && !is.null(lst$flowpaths)) {
      crs1 <- sf::st_crs(lst[[1]])
      crs2 <- sf::st_crs(lst[[2]])
      if (!identical(crs1, crs2) && (!is.na(crs1$epsg) || !is.na(crs1$wkt))) {
        lst[[2]] <- sf::st_transform(lst[[2]], crs1)
        if (isTRUE(verbose)) cli::cli_alert_info("Harmonized CRS: transformed second layer to match the first.")
      }
    }
    lst
  }

  .transform_all <- function(lst, target_crs) {
    lapply(lst, function(x) if (!is.null(x)) sf::st_transform(x, target_crs) else NULL)
  }

  # ---- main ----------------------------------------------------------------
  out <- list()

  if (is.null(gpkg)) {
    if (.is_sf(divides))   out$divides   <- divides
    if (.is_sf(flowpaths)) out$flowpaths <- flowpaths
    if (length(out) == 0L) {
      cli::cli_abort("Nothing to read: provide a GeoPackage path or in-memory `sf` objects.")
    }
  } else {
    gpkg <- normalizePath(gpkg, mustWork = TRUE)
    if (isTRUE(verbose)) cli::cli_alert_info("\n--- Reading Hydrofabric from {.path {gpkg}} ---\n")

    want_divides   <- realization %in% c("all", "divides")
    want_flowpaths <- realization %in% c("all", "flowpaths")

    if (want_flowpaths) {
      if (.is_sf(flowpaths)) {
        out$flowpaths <- flowpaths
      } else {
        if (is.null(flowpaths)) flowpaths <- .autodetect_flowpaths(gpkg)
        if (!is.null(flowpaths)) out$flowpaths <- .read_layer(gpkg, flowpaths, "flowpaths")
      }
    }

    if (want_divides) {
      if (.is_sf(divides)) {
        out$divides <- divides
      } else {
        if (is.null(divides)) divides <- .autodetect_divides(gpkg)
        if (!is.null(divides)) out$divides <- .read_layer(gpkg, divides, "divides")
      }
    }

    if (length(out) == 0L) {
      cli::cli_abort("No layers were read. Check `realization` and layer names in the GeoPackage.")
    }
  }

  if (!is.null(crs)) {
    out <- .transform_all(out, crs)
    if (isTRUE(verbose)) cli::cli_alert_info("Transformed output to requested CRS.")
  } else {
    out <- .harmonize_crs_pair(out)
  }

  if (isTRUE(verbose)) {
    have <- paste(names(out), collapse = " & ")
    cli::cli_alert_success("Done. Returned: {have}.")
  }

  out
}


#' Write a hydrofabric GeoPackage (mixed sf + non-sf)
#'
#' @param network_list named list of layers (may include `sf` and plain data.frames)
#' @param outfile path to `.gpkg` (".gpkg" appended if missing)
#' @param verbose logical, show progress via `cli`
#' @param enforce_dm logical, enforce `hf_dm` schema (column presence)
#' @return `outfile` (invisibly)
#' @export
write_hydrofabric <- function(network_list,
                              outfile,
                              verbose = TRUE,
                              enforce_dm = TRUE) {
  say <- function(fn, msg) if (isTRUE(verbose)) fn(msg)

  if (!is.list(network_list) || length(network_list) == 0)
    stop("`network_list` must be a non-empty named list.")

  if (is.null(names(network_list)) || any(names(network_list) == "" | is.na(names(network_list))))
    stop("All elements of `network_list` must be *named* (these become layer/table names).")

  if (!is.character(outfile) || length(outfile) != 1L)
    stop("`outfile` must be a single file path.")

  if (!grepl("\\.gpkg$", outfile, ignore.case = TRUE))
    outfile <- paste0(outfile, ".gpkg")

  outdir <- dirname(outfile)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # sanitize layer names for SQLite/GPKG
  clean_name <- function(x) gsub("[^A-Za-z0-9_]+", "_", x)
  layer_names <- setNames(clean_name(names(network_list)), names(network_list))

  # ---- schema enforcement (columns only; types optional) ----
  if (isTRUE(enforce_dm)) {
    if (!exists("hf_dm", inherits = TRUE))
      stop("`enforce_dm = TRUE` but `hf_dm` not found in scope.")
    dm <- get("hf_dm", inherits = TRUE)
    # If WB not present, drop `wb_id` requirement from all specs
    if (!"WB" %in% names(network_list)) {
      dm <- lapply(dm, function(df) df[, setdiff(names(df), "wb_id"), drop = FALSE])
    }
    enforce_cols <- function(data, layer_key, layer_name) {
      spec <- NULL
      if (layer_key %in% names(dm)) spec <- dm[[layer_key]]
      if (is.null(spec) && identical(layer_key, "nexus")) {
        spec <- data.frame(id = double(), toid = double(), hl_id = double(), type = character())
      }
      if (!is.null(spec)) {
        need <- names(spec)
        have <- names(data)
        missing <- setdiff(need, have)
        if (length(missing))
          stop("Layer/table `", layer_name, "` is missing required columns: ",
               paste(missing, collapse = ", "), call. = FALSE)
      }
      TRUE
    }
  } else {
    enforce_cols <- function(...) TRUE
  }

  # ---- partition by type ----
  is_sf <- vapply(network_list, function(x) inherits(x, "sf"), logical(1))
  sf_layers   <- network_list[is_sf]
  tab_layers  <- network_list[!is_sf]

  # ---- Write atomically to a temp gpkg ----
  tmpfile <- tempfile(fileext = ".gpkg")

  wrote_any <- FALSE

  # 1) If we have any sf layers, use write_sf to create the GPKG
  if (length(sf_layers)) {
    say(cli::cli_alert_info, glue::glue("Writing {length(sf_layers)} spatial layer(s) to temp GPKG..."))
    i <- 0L
    for (nm in names(sf_layers)) {
      obj <- sf_layers[[nm]]
      nm_out <- layer_names[[nm]]
      enforce_cols(obj, switch(nm_out,
                               flowpaths = "flowlines", # your dm key → layer mapping
                               divides   = "divides",
                               pois      = "pois",
                               network   = "network",
                               WB        = "WB",
                               nexus     = "nexus",
                               nm_out),
                   nm_out)
      i <- i + 1L
      sf::write_sf(obj,
                   dsn = tmpfile,
                   layer = nm_out)
      wrote_any <- TRUE
    }
  }

  # 2) Write non-spatial tables with DBI
  if (length(tab_layers)) {
    say(cli::cli_alert_info, glue::glue("Writing {length(tab_layers)} attribute table(s)..."))

    # If no sf written yet, ensure a proper GeoPackage header
    # by creating a SQLite file and tagging application_id = 1196437808 ('GPKG')
    if (!wrote_any) {
      # create empty SQLite file
      con0 <- DBI::dbConnect(RSQLite::SQLite(), tmpfile)
      on.exit(try(DBI::dbDisconnect(con0), silent = TRUE), add = TRUE)
      DBI::dbExecute(con0, "PRAGMA application_id=1196437808") # 'GPKG'
      DBI::dbDisconnect(con0)
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), tmpfile)
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

    for (nm in names(tab_layers)) {
      obj <- tab_layers[[nm]]
      nm_out <- layer_names[[nm]]
      if (!is.data.frame(obj))
        stop("Non-sf layer `", nm, "` must be a data.frame/tibble.")

      enforce_cols(obj, nm_out, nm_out)

      # Overwrite if exists
      if (DBI::dbExistsTable(con, nm_out))
        DBI::dbRemoveTable(con, nm_out)

      DBI::dbWriteTable(con, nm_out, obj)
      wrote_any <- TRUE
    }
    DBI::dbDisconnect(con)
  }

  if (!wrote_any)
    stop("No layers/tables were written. Check `network_list` contents.")

  # ---- atomic swap ----
  if (file.exists(outfile)) unlink(outfile)
  ok <- file.rename(tmpfile, outfile)
  if (!ok) {
    ok <- file.copy(tmpfile, outfile, overwrite = TRUE)
    unlink(tmpfile)
    if (!ok) stop("Failed to finalize write to `", outfile, "`.")
  }

  say(cli::cli_alert_success, glue::glue("Wrote {length(network_list)} layer(s)/table(s) → {outfile}"))
  invisible(normalizePath(outfile))
}

# small helper for `%||%`
`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && identical(a, ""))) b else a
