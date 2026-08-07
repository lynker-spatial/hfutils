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
#'   variable `hfutils_verbose` (anything other than literal `"false"` is treated
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
                             verbose = Sys.getenv("hfutils_verbose") != "false") {

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
    if (.is_sf(divides)) out$divides   <- divides
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
#' @description
#' Writes a named list of layers to a single GeoPackage, mixing `sf` layers and
#' plain attribute tables. The write is atomic: layers are staged in a temporary
#' GeoPackage and only moved into place once every layer has been written.
#'
#' @details
#' When the list carries a flowpath topology (a layer with `flowpath_id` and
#' `flowpath_toid`), a nested-set upstream index is computed with
#' [hf_upstream_index()] and stamped onto every flowpath-keyed layer as the
#' integer columns `upstream_id` and `num_upstreams`, so the written GeoPackage
#' supports O(1) upstream range queries. A nexus layer (`nexus_id` /
#' `nexus_toid`), if present, is used to resolve `flowpath -> nexus -> flowpath`
#' hops. The step is attribute-only and is skipped without failing the write
#' when the topology is absent or not acyclic. Because the index is derived from
#' whatever topology is in `network_list`, per-VPU, merged, and subset writes
#' each get a correct index for their own scope; the values are build-specific
#' and are not persistent keys.
#'
#' @param network_list named list of layers (may include `sf` and plain data.frames)
#' @param outfile path to `.gpkg` (".gpkg" appended if missing)
#' @param verbose logical, show progress via `cli`
#' @param enforce_dm logical, enforce a data-model schema (column presence) by
#'   validating each layer against an `hf_dm` object found in scope. Defaults to
#'   `FALSE`; `hf_dm` is not shipped with hfutils, so enable this only when a
#'   caller (e.g. the `hydrofabric` build package) provides `hf_dm`.
#' @return `outfile` (invisibly)
#' @examples
#' \dontrun{
#' network_list <- list(
#'   flowpaths = sf::read_sf("in.gpkg", "flowpaths"),
#'   divides   = sf::read_sf("in.gpkg", "divides"),
#'   network   = sf::st_drop_geometry(sf::read_sf("in.gpkg", "flowpaths"))
#' )
#' write_hydrofabric(network_list, "hydrofabric.gpkg")
#' }
#' @export
write_hydrofabric <- function(network_list,
                              outfile,
                              verbose = TRUE,
                              enforce_dm = FALSE) {
  say <- function(fn, msg) if (isTRUE(verbose)) fn(msg)

  if (!is.list(network_list) || length(network_list) == 0)
    cli::cli_abort("{.arg network_list} must be a non-empty named list.")

  if (is.null(names(network_list)) || any(names(network_list) == "" | is.na(names(network_list))))
    cli::cli_abort("All elements of {.arg network_list} must be named (these become layer/table names).")

  if (!is.character(outfile) || length(outfile) != 1L)
    cli::cli_abort("{.arg outfile} must be a single file path.")

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
      cli::cli_abort(c(
        "{.arg enforce_dm} is {.code TRUE} but {.var hf_dm} was not found in scope.",
        "i" = "Supply an {.var hf_dm} data-model object, or set {.code enforce_dm = FALSE}."))
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
          cli::cli_abort("Layer/table {.val {layer_name}} is missing required column{?s}: {.field {missing}}.")
      }
      TRUE
    }
  } else {
    enforce_cols <- function(data, layer_key, layer_name) TRUE
  }

  # Stamp the nested-set upstream index on every flowpath-keyed layer when the
  # list is a complete hydrofabric, so the written gpkg carries a correct-scope
  # O(1) upstream index (see .hf_stamp_upstream_index). Attribute-only.
  network_list <- .hf_stamp_upstream_index(network_list, say)

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
        flowpaths = "flowlines", # dm key -> layer name mapping
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
      on.exit(suppressWarnings(try(DBI::dbDisconnect(con0), silent = TRUE)), add = TRUE)
      DBI::dbExecute(con0, "PRAGMA application_id=1196437808") # 'GPKG'
      DBI::dbDisconnect(con0)
    }

    con <- DBI::dbConnect(RSQLite::SQLite(), tmpfile)
    on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

    for (nm in names(tab_layers)) {
      obj <- tab_layers[[nm]]
      nm_out <- layer_names[[nm]]
      if (!is.data.frame(obj))
        cli::cli_abort("Non-sf layer {.val {nm}} must be a data.frame/tibble.")

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
    cli::cli_abort("No layers/tables were written. Check {.arg network_list} contents.")

  # ---- atomic swap ----
  if (file.exists(outfile)) unlink(outfile)
  ok <- file.rename(tmpfile, outfile)
  if (!ok) {
    ok <- file.copy(tmpfile, outfile, overwrite = TRUE)
    unlink(tmpfile)
    if (!ok) cli::cli_abort("Failed to finalize write to {.path {outfile}}.")
  }

  say(cli::cli_alert_success, glue::glue("Wrote {length(network_list)} layer(s)/table(s) -> {outfile}"))
  invisible(normalizePath(outfile))
}

# Stamp the nested-set upstream index onto every flowpath-keyed layer in a
# hydrofabric network_list. Schema-detected (finds the flowpath and nexus layers
# by their id/toid columns), attribute-only, and a no-op when the topology is
# absent or the network is not acyclic. Shared by write_hydrofabric so per-VPU,
# merged, and subset writes all carry a correct-scope index.
.hf_stamp_upstream_index <- function(network_list, say = function(fn, msg) NULL) {
  .find <- function(nl, cols) {
    for (nm in names(nl)) {
      x <- nl[[nm]]
      if (is.data.frame(x) && all(cols %in% names(x))) return(nm)
    }
    NULL
  }
  fp_nm <- .find(network_list, c("flowpath_id", "flowpath_toid"))
  if (is.null(fp_nm)) return(network_list)
  nex_nm <- .find(network_list, c("nexus_id", "nexus_toid"))

  fp  <- network_list[[fp_nm]]
  nex <- if (!is.null(nex_nm)) network_list[[nex_nm]] else NULL
  if (inherits(fp, "sf")) fp <- sf::st_drop_geometry(fp)
  if (!is.null(nex) && inherits(nex, "sf")) nex <- sf::st_drop_geometry(nex)

  tbl <- tryCatch(hf_upstream_index(fp, nex), error = function(e) {
    say(cli::cli_alert_warning, glue::glue("upstream index skipped: {conditionMessage(e)}"))
    NULL
  })
  if (is.null(tbl) || nrow(tbl) == 0L) return(network_list)

  lu_u <- stats::setNames(tbl$upstream_id,   tbl$flowpath_id)
  lu_n <- stats::setNames(tbl$num_upstreams, tbl$flowpath_id)
  n_stamped <- 0L
  for (nm in names(network_list)) {
    x <- network_list[[nm]]
    if (!is.data.frame(x) || !("flowpath_id" %in% names(x))) next
    key <- as.character(x$flowpath_id)
    x$upstream_id   <- as.integer(unname(lu_u[key]))
    x$num_upstreams <- as.integer(unname(lu_n[key]))
    network_list[[nm]] <- x
    n_stamped <- n_stamped + 1L
  }
  say(cli::cli_alert_info, glue::glue(
    "Upstream index: {nrow(tbl)} flowpaths across {attr(tbl, 'n_outlets')} outlet(s), stamped on {n_stamped} layer(s)"))
  network_list
}
