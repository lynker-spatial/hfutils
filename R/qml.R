#' Read a QML style file
#'
#' @param qml_file Path to a `.qml` file.
#' @return The file contents as a single character string.
#' @examples
#' qml <- read_qml(system.file("qml", "nexus.qml", package = "hfutils"))
#' substr(qml, 1, 40)
#' @export
read_qml <- function(qml_file) {
  paste(readLines(qml_file, warn = FALSE), collapse = "\n")
}

#' Build one `layer_styles` row for a GeoPackage layer
#'
#' @description
#' Assembles a single row in the schema QGIS expects in a GeoPackage's
#' `layer_styles` table. The geometry column is looked up from
#' `gpkg_geometry_columns`, so a layer without geometry (an attribute table such
#' as `network`) yields `NULL` rather than an error: QGIS styles apply to
#' spatial layers only.
#'
#' @param gpkg_path Path to the GeoPackage.
#' @param layer_name Layer the style applies to.
#' @param style_name Name recorded for the style.
#' @param style_qml QML contents, from [read_qml()].
#' @return A one-row data frame, or `NULL` if `layer_name` has no geometry
#'   column in `gpkg_path`.
#' @examples
#' \dontrun{
#' row <- create_style_row("hydrofabric.gpkg", "nexus", "nexus_style",
#'   read_qml(system.file("qml", "nexus.qml", package = "hfutils")))
#' }
#' @export
create_style_row <- function(gpkg_path, layer_name, style_name, style_qml) {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg_path)
  on.exit(suppressWarnings(try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

  geom <- DBI::dbGetQuery(con,
    "SELECT column_name FROM gpkg_geometry_columns WHERE table_name = ?",
    params = list(layer_name))

  if (!nrow(geom)) return(NULL)

  data.frame(
    f_table_catalog   = "",
    f_table_schema    = "",
    f_table_name      = layer_name,
    f_geometry_column = geom[[1]][1],
    styleName         = style_name,
    styleQML          = style_qml,
    styleSLD          = "",
    useAsDefault      = TRUE,
    description       = "Generated for hydrofabric",
    owner             = "",
    ui                = NA_character_,
    update_time       = Sys.time(),
    stringsAsFactors  = FALSE
  )
}

#' Write QGIS layer styles into a GeoPackage
#'
#' @description
#' Stamps the packaged QML symbology into a GeoPackage's `layer_styles` table,
#' so the file opens pre-styled in QGIS. One row is written per requested layer
#' that has both a shipped QML and a geometry column; any existing
#' `layer_styles` table is replaced.
#'
#' @details
#' QML files are matched to layers by exact basename, so `flowpaths` takes
#' `flowpaths.qml` and never `flowlines.qml`. Requested layers with no shipped
#' QML, and attribute tables with no geometry column, are skipped rather than
#' mismatched or errored on.
#'
#' `layer_styles` is a QGIS extension rather than part of the GeoPackage
#' specification, so it appears as an extra table in the file. [as_ogr()]
#' excludes it from layer discovery by default.
#'
#' @param gpkg_path Path to the GeoPackage to stamp.
#' @param qml_dir Directory of `.qml` files. Defaults to the set shipped with
#'   hfutils (`divides`, `flowlines`, `flowpaths`, `hydrolocations`, `lakes`,
#'   `nexus`).
#' @param layer_names Layers to style.
#' @return `gpkg_path`, invisibly.
#' @examples
#' \dontrun{
#' append_style("hydrofabric.gpkg", layer_names = c("divides", "flowpaths"))
#' }
#' @export
append_style <- function(gpkg_path,
                         qml_dir = system.file("qml", package = "hfutils"),
                         layer_names) {

  if (!file.exists(gpkg_path)) cli::cli_abort("{.path {gpkg_path}} does not exist.")

  qml <- list.files(qml_dir, pattern = "[.]qml$", full.names = TRUE)
  if (!length(qml)) cli::cli_abort("No QML files found in {.path {qml_dir}}.")
  names(qml) <- sub("[.]qml$", "", basename(qml))

  # Match on exact basename and index by layer name throughout. Pairing the
  # caller's layer order against a directory listing would attach each style to
  # whichever layer happened to sort into the same position.
  wanted <- unique(as.character(layer_names))
  styled <- wanted[wanted %in% names(qml)]

  rows <- lapply(styled, function(lyr) {
    create_style_row(gpkg_path, lyr, paste0(lyr, "__hydrofabric_style"),
      read_qml(qml[[lyr]]))
  })
  rows <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])

  if (is.null(rows) || !nrow(rows)) return(invisible(gpkg_path))

  if ("layer_styles" %in% sf::st_layers(gpkg_path)$name) {
    try(sf::st_delete(gpkg_path, "layer_styles", quiet = TRUE), silent = TRUE)
  }

  sf::st_write(rows, gpkg_path, layer = "layer_styles", append = FALSE,
    quiet = TRUE)

  invisible(gpkg_path)
}
