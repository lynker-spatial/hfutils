#' Clean polygon topology in a GeoPackage layer using GDAL and Mapshaper
#'
#' Extracts a polygon layer from a GeoPackage, reprojects it to EPSG:4326,
#' runs `mapshaper -clean` to repair polygon topology, reprojects the cleaned
#' output back to the original coordinate reference system, and overwrites the
#' original layer in the source GeoPackage.
#'
#' This function is intended for polygon datasets that may contain small
#' overlaps, enclosed gaps, or minor boundary misalignments that prevent a
#' clean polygon mosaic. The topology repair is performed in geographic
#' coordinates because `mapshaper -clean` is often more reliable there for
#' mixed-source inputs, after which the data are restored to their original CRS.
#'
#' The function relies on external command line tools:
#' \itemize{
#'   \item `ogr2ogr` for layer extraction, reprojection, and overwrite
#'   \item `gdalsrsinfo` for recovering the original CRS definition
#'   \item `mapshaper` for polygon cleaning
#' }
#'
#' CRS detection prefers an EPSG code, then a PROJ string, and finally a simple
#' WKT representation as a fallback.
#'
#' @param gpkg Character scalar. Path to the input GeoPackage.
#' @param layer Character scalar. Name of the layer in `gpkg` to clean.
#' @param mapshaper Character scalar. Path to the `mapshaper` executable.
#'   Defaults to `"mapshaper"`, assuming it is available on the system path.
#' @param ogr2ogr Character scalar. Path to the `ogr2ogr` executable.
#'   Defaults to `"ogr2ogr"`.
#' @param gdalsrsinfo Character scalar. Path to the `gdalsrsinfo` executable.
#'   Defaults to `"gdalsrsinfo"`.
#' @param snap_interval Optional numeric or character scalar. Passed to
#'   `mapshaper -clean` as `snap-interval=...`. Use this to snap nearby
#'   vertices before topology repair. Because cleaning occurs in EPSG:4326,
#'   this value is interpreted in decimal degrees.
#' @param gap_fill_area Optional numeric or character scalar. Passed to
#'   `mapshaper -clean` as `gap-fill-area=...`. Controls the maximum enclosed
#'   gap area that will be filled.
#' @param overlap_rule Optional character scalar. Passed to `mapshaper -clean`
#'   as `overlap-rule=...`. Controls how overlap areas are assigned. Common
#'   values include `"max-area"`, `"min-area"`, `"max-id"`, and `"min-id"`.
#' @param verbose Logical scalar. If `TRUE`, print step-by-step progress,
#'   commands, and captured CRS detection output.
#'
#' @return Invisibly returns a named list with:
#' \describe{
#'   \item{gpkg}{Path to the GeoPackage that was modified.}
#'   \item{layer}{Layer name that was cleaned.}
#'   \item{original_srs}{The CRS definition used to project the cleaned layer
#'   back to its original coordinate system.}
#'   \item{tmp_files}{Named character vector of temporary intermediate files
#'   used during processing. These are scheduled for deletion on exit.}
#' }
#'
#' @details
#' The processing steps are:
#' \enumerate{
#'   \item Extract the requested layer from the source GeoPackage into a
#'     temporary single-layer GeoPackage.
#'   \item Detect the original CRS using `gdalsrsinfo`, preferring EPSG output.
#'   \item Reproject the temporary layer to EPSG:4326 and write it as GeoJSON.
#'   \item Run `mapshaper -clean` on the EPSG:4326 GeoJSON.
#'   \item Reproject the cleaned output back to the original CRS.
#'   \item Overwrite the original layer in the input GeoPackage.
#' }
#'
#' Temporary files are created in `tempdir()` and removed with `on.exit()`.
#'
#' The function assumes the target layer contains polygonal data appropriate
#' for `mapshaper -clean`. It may still run on other geometry types, but the
#' repair behavior is designed for polygon topology.
#'
#' @section Requirements:
#' The following executables must be installed and available:
#' \itemize{
#'   \item GDAL tools: `ogr2ogr`, `gdalsrsinfo`
#'   \item Mapshaper CLI
#' }
#'
#' On many systems, Mapshaper can be installed with:
#' \preformatted{
#' npm install -g mapshaper
#' }
#'
#' @section Caveats:
#' \itemize{
#'   \item Cleaning is performed in EPSG:4326, so `snap_interval` is interpreted
#'   in angular units, not the source layer's native units.
#'   \item Overwriting replaces the original layer in the GeoPackage.
#'   \item Attribute preservation depends on the behavior of `ogr2ogr` and
#'   `mapshaper`; field ordering and some metadata may change.
#'   \item Very large layers may require substantial temporary disk space.
#' }
#'
#' @examples
#' \dontrun{
#' clean_gpkg_layer(
#'   gpkg = "data/hydrofabric.gpkg",
#'   layer = "divides"
#' )
#'
#' clean_gpkg_layer(
#'   gpkg = "data/hydrofabric.gpkg",
#'   layer = "catchments",
#'   snap_interval = 1e-7,
#'   overlap_rule = "max-area",
#'   verbose = TRUE
#' )
#'
#' clean_gpkg_layer(
#'   gpkg = "data/hydrofabric.gpkg",
#'   layer = "basins",
#'   gap_fill_area = 0,
#'   mapshaper = "/usr/local/bin/mapshaper",
#'   ogr2ogr = "/opt/homebrew/bin/ogr2ogr",
#'   gdalsrsinfo = "/opt/homebrew/bin/gdalsrsinfo"
#' )
#' }
#'
#' @export
clean_gpkg_layer <- function(
    gpkg,
    layer,
    mapshaper = "mapshaper",
    ogr2ogr = "ogr2ogr",
    gdalsrsinfo = "gdalsrsinfo",
    snap_interval = NULL,
    gap_fill_area = NULL,
    overlap_rule = NULL,
    verbose = FALSE
) {
  stopifnot(file.exists(gpkg))

  tmp_dir <- tempdir()
  # Process-unique stem: forked parallel workers (e.g. concurrent gage runs via
  # mclapply) share one tempdir(), so layer-keyed names alone would collide and
  # clobber each other's intermediates. The PID separates workers; the tempfile
  # suffix separates repeated calls within a worker.
  uniq      <- sprintf("%s_p%d_%s", layer, Sys.getpid(),
                       sub("^file", "", basename(tempfile(""))))
  tmp_src   <- file.path(tmp_dir, paste0(uniq, "_src.gpkg"))
  tmp_4326  <- file.path(tmp_dir, paste0(uniq, "_4326.geojson"))
  tmp_clean <- file.path(tmp_dir, paste0(uniq, "_4326_clean.geojson"))
  tmp_back  <- file.path(tmp_dir, paste0(uniq, "_back.gpkg"))

  log_msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  on.exit({
    log_msg("\n[cleanup] removing temporary files")
    # Also clean up any numbered files mapshaper may have written
    # (e.g. layer_4326_clean1.geojson, layer_4326_clean2.geojson)
    stem    <- sub("\\.geojson$", "", tmp_clean)
    numbered <- list.files(dirname(tmp_clean),
                           pattern = paste0("^", basename(stem), "[0-9]+\\.geojson$"),
                           full.names = TRUE)
    unlink(c(tmp_src, tmp_4326, tmp_clean, tmp_back, numbered), force = TRUE)
  }, add = TRUE)

  run_cmd <- function(cmd, args, capture = FALSE, step = NULL) {
    if (!is.null(step)) {
      log_msg("\n[step] ", step)
    }

    log_msg("[cmd] ", cmd, " ", paste(shQuote(args), collapse = " "))

    if (capture) {
      out <- system2(cmd, args = args, stdout = TRUE, stderr = TRUE)
      status <- attr(out, "status")
      if (length(out)) {
        log_msg("[output]\n", paste(out, collapse = "\n"))
      }
      if (!is.null(status) && status != 0) {
        stop(
          sprintf("Command failed [%s]\n%s", cmd, paste(out, collapse = "\n")),
          call. = FALSE
        )
      }
      return(out)
    } else {
      status <- system2(cmd, args = args)
      if (!identical(status, 0L)) {
        stop(sprintf("Command failed [%s] with exit status %s", cmd, status), call. = FALSE)
      }
      invisible(status)
    }
  }

  get_srs_def <- function(src) {
    log_msg("\n[info] detecting original CRS")

    epsg <- tryCatch(
      paste(run_cmd(
        gdalsrsinfo,
        c("-o", "epsg", src),
        capture = TRUE,
        step = "Read CRS as EPSG"
      ), collapse = " "),
      error = function(e) ""
    )
    epsg <- trimws(epsg)
    epsg <- sub("^EPSG:\\s*", "EPSG:", epsg)
    epsg <- sub("^ESRI:\\s*", "ESRI:", epsg)

    if (grepl("^(EPSG|ESRI):[0-9]+$", epsg)) {
      log_msg("[info] using CRS: ", epsg)
      return(epsg)
    }

    proj4 <- tryCatch(
      paste(run_cmd(
        gdalsrsinfo,
        c("-o", "proj4", src),
        capture = TRUE,
        step = "Read CRS as PROJ string"
      ), collapse = " "),
      error = function(e) ""
    )
    proj4 <- trimws(proj4)

    if (nzchar(proj4) && !grepl("not found|ERROR", proj4, ignore.case = TRUE)) {
      log_msg("[info] using CRS: ", proj4)
      return(proj4)
    }

    wkt <- tryCatch(
      paste(run_cmd(
        gdalsrsinfo,
        c("-o", "wkt_simple", src),
        capture = TRUE,
        step = "Read CRS as WKT"
      ), collapse = " "),
      error = function(e) ""
    )
    wkt <- trimws(wkt)

    if (nzchar(wkt)) {
      log_msg("[info] using CRS: ", wkt)
      return(wkt)
    }

    stop("Could not determine source CRS.", call. = FALSE)
  }

  log_msg("[start] cleaning layer '", layer, "' in ", gpkg)
  log_msg("[temp] tmp_src   = ", tmp_src)
  log_msg("[temp] tmp_4326  = ", tmp_4326)
  log_msg("[temp] tmp_clean = ", tmp_clean)
  log_msg("[temp] tmp_back  = ", tmp_back)

  run_cmd(
    ogr2ogr,
    c("-f", "GPKG", tmp_src, gpkg, layer),
    step = "Extract source layer"
  )

  orig_srs <- get_srs_def(tmp_src)

  run_cmd(
    ogr2ogr,
    c("-f", "GeoJSON", "-t_srs", "EPSG:4326", tmp_4326, tmp_src),
    step = "Reproject to EPSG:4326"
  )

  clean_args <- c(tmp_4326, "-clean")

  if (!is.null(snap_interval)) {
    clean_args <- c(clean_args, sprintf("snap-interval=%s", snap_interval))
  }
  if (!is.null(gap_fill_area)) {
    clean_args <- c(clean_args, sprintf("gap-fill-area=%s", gap_fill_area))
  }
  if (!is.null(overlap_rule)) {
    clean_args <- c(clean_args, sprintf("overlap-rule=%s", overlap_rule))
  }

  clean_args <- c(clean_args, "-o", tmp_clean)

  run_cmd(
    mapshaper,
    clean_args,
    step = "Run mapshaper clean"
  )

  # mapshaper writes numbered files (e.g. layer_4326_clean1.geojson,
  # layer_4326_clean2.geojson) when the input contains null-geometry features
  # or mixed geometry types treated as separate layers.  The first numbered
  # file always holds the polygon features; subsequent files hold residuals
  # (null geometries, non-polygon parts) that are irrelevant for divide
  # cleaning.  Use file.rename to promote the first numbered file to the
  # expected output path, bypassing an ogr2ogr merge that fails on GeoJSON
  # with non-standard geometry types.
  if (!file.exists(tmp_clean)) {
    stem     <- sub("\\.geojson$", "", tmp_clean)
    numbered <- list.files(dirname(tmp_clean),
                           pattern = paste0("^", basename(stem), "[0-9]+\\.geojson$"),
                           full.names = TRUE)
    if (length(numbered) > 0L) {
      log_msg("[info] mapshaper wrote ", length(numbered),
              " numbered file(s) — using first (polygon) file as output")
      if (!file.rename(numbered[[1L]], tmp_clean)) {
        file.copy(numbered[[1L]], tmp_clean, overwrite = TRUE)
      }
    } else {
      stop(sprintf("mapshaper did not write output to '%s' (no numbered files found either)",
                   tmp_clean),
           call. = FALSE)
    }
  }

  run_cmd(
    ogr2ogr,
    c("-f", "GPKG", "-t_srs", orig_srs, tmp_back, tmp_clean, "-nln", layer),
    step = "Reproject cleaned layer back to original CRS"
  )

  run_cmd(
    ogr2ogr,
    c("-f", "GPKG", gpkg, tmp_back, "-nln", layer, "-overwrite"),
    step = "Overwrite original layer"
  )

  log_msg("\n[done] successfully cleaned layer '", layer, "'")

  invisible(list(
    gpkg = gpkg,
    layer = layer,
    original_srs = orig_srs,
    tmp_files = c(
      tmp_src = tmp_src,
      tmp_4326 = tmp_4326,
      tmp_clean = tmp_clean,
      tmp_back = tmp_back
    )
  ))
}
