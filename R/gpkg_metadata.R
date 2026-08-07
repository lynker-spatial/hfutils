#' Convert a semantic version string to a packed integer (`Mmmpp`)
#'
#' `"2.1.4"` -> `20104` (major * 10000 + minor * 100 + patch). Minor and patch
#' are expected to be < 100. A leading `v` and any pre-release / build suffix
#' (after `-` or `+`) are ignored.
#'
#' @param version Character semantic-version string, e.g. `"2.1.4"` or `"v2.1.4"`.
#' @return A single integer.
#' @noRd
semver_to_int <- function(version) {
  v <- gsub("^v", "", trimws(version))
  p <- suppressWarnings(as.integer(strsplit(v, "[.+-]")[[1]]))
  p <- c(p, 0L, 0L, 0L)[1:3]
  p[is.na(p)] <- 0L
  p[1] * 10000L + p[2] * 100L + p[3]
}

#' Stamp a dataset version into a GeoPackage's metadata tables
#'
#' Records the dataset version in the standard GeoPackage metadata extension
#' tables (`gpkg_metadata` / `gpkg_metadata_reference`), creating and registering
#' the extension if the GeoPackage does not already have it. Three entries are
#' written at `geopackage` scope:
#'
#' \itemize{
#'   \item a machine-readable integer version (`Mmmpp`, `text/plain`), surfaced
#'     by GDAL as `GPKG_METADATA_ITEM_1`;
#'   \item a human-readable semantic version (`text/plain`);
#'   \item if `provenance` is supplied, a JSON build-provenance record
#'     (`application/json`).
#' }
#'
#' The dataset version is independent of the GeoPackage *specification* version
#' held in `PRAGMA user_version`, which is left untouched. The function is
#' idempotent: re-stamping replaces any prior entries it wrote rather than
#' accumulating duplicates.
#'
#' @param gpkg Path to a GeoPackage file.
#' @param version Human-readable semantic version, e.g. `"2.1.4"`.
#' @param int_version Machine-readable integer version. Defaults to the packed
#'   `Mmmpp` form of `version`.
#' @param provenance Optional named `list` written as a JSON build-provenance
#'   entry (for example `list(software = "hydrofabric 0.1", git_sha = "...",
#'   build_date = "...")`). `NULL` (default) writes no provenance entry.
#' @param license Optional SPDX license identifier (e.g. `"CC0-1.0"`,
#'   `"ODbL-1.0"`) recorded as a metadata entry. `NULL` (default) writes none.
#' @param scope GeoPackage metadata reference scope. Default `"geopackage"`
#'   (whole-dataset).
#'
#' @return The `gpkg` path, invisibly.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite toJSON
#' @examples
#' \dontrun{
#' gpkg_set_version("hydrofabric.gpkg", version = "2.2.0",
#'   license = "ODbL-1.0",
#'   provenance = list(software = "hydrofabric 0.1",
#'     build_date = Sys.Date()))
#' }
#' @export
gpkg_set_version <- function(gpkg, version, int_version = semver_to_int(version),
                             provenance = NULL, license = NULL, scope = "geopackage") {

  stopifnot(file.exists(gpkg), length(version) == 1L, !is.na(version))

  prov_uri <- "https://lynker-spatial.com/ns/hydrofabric-provenance"
  spdx_uri <- "https://spdx.org/licenses/"
  uris     <- c("https://schema.org", "https://semver.org", prov_uri, spdx_uri)

  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tbls <- DBI::dbListTables(con)

  if (!"gpkg_metadata" %in% tbls) {
    DBI::dbExecute(con, "CREATE TABLE gpkg_metadata (
      id INTEGER CONSTRAINT m_pk PRIMARY KEY ASC NOT NULL,
      md_scope TEXT NOT NULL DEFAULT 'dataset',
      md_standard_uri TEXT NOT NULL,
      mime_type TEXT NOT NULL DEFAULT 'text/xml',
      metadata TEXT NOT NULL DEFAULT '')")
  }
  if (!"gpkg_metadata_reference" %in% tbls) {
    DBI::dbExecute(con, "CREATE TABLE gpkg_metadata_reference (
      reference_scope TEXT NOT NULL,
      table_name TEXT, column_name TEXT, row_id_value INTEGER,
      timestamp DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')),
      md_file_id INTEGER NOT NULL,
      md_parent_id INTEGER,
      CONSTRAINT crmr_mfi_fk FOREIGN KEY (md_file_id) REFERENCES gpkg_metadata(id),
      CONSTRAINT crmr_mpi_fk FOREIGN KEY (md_parent_id) REFERENCES gpkg_metadata(id))")
  }

  # Register the metadata extension (spec-strict: one row per metadata table).
  if ("gpkg_extensions" %in% tbls) {
    def <- "http://www.geopackage.org/spec/#extension_metadata"
    for (t in c("gpkg_metadata", "gpkg_metadata_reference")) {
      n <- DBI::dbGetQuery(con, "SELECT count(*) n FROM gpkg_extensions
        WHERE extension_name='gpkg_metadata' AND table_name=?", params = list(t))$n
      if (n == 0L)
        DBI::dbExecute(con, "INSERT INTO gpkg_extensions
          (table_name,column_name,extension_name,definition,scope)
          VALUES (?, NULL, 'gpkg_metadata', ?, 'read-write')", params = list(t, def))
    }
  }

  # Idempotent: drop any entries this function previously wrote.
  ph <- paste(rep("?", length(uris)), collapse = ",")
  DBI::dbExecute(con, sprintf(
    "DELETE FROM gpkg_metadata_reference WHERE md_file_id IN
       (SELECT id FROM gpkg_metadata WHERE md_standard_uri IN (%s))", ph),
  params = as.list(uris))
  DBI::dbExecute(con, sprintf(
    "DELETE FROM gpkg_metadata WHERE md_standard_uri IN (%s)", ph),
  params = as.list(uris))

  add <- function(uri, mime, value) {
    DBI::dbExecute(con, "INSERT INTO gpkg_metadata
      (md_scope, md_standard_uri, mime_type, metadata) VALUES ('dataset', ?, ?, ?)",
      params = list(uri, mime, as.character(value)))
    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() id")$id
    DBI::dbExecute(con, "INSERT INTO gpkg_metadata_reference
      (reference_scope, md_file_id) VALUES (?, ?)", params = list(scope, id))
  }

  add("https://schema.org", "text/plain", int_version)   # machine (Mmmpp)
  add("https://semver.org", "text/plain", version)        # human (semver)
  if (!is.null(provenance)) {
    add(prov_uri, "application/json",
      jsonlite::toJSON(provenance, auto_unbox = TRUE, null = "null"))
  }
  if (!is.null(license)) add(spdx_uri, "text/plain", license)   # SPDX identifier

  invisible(gpkg)
}

#' Read the dataset version written by [gpkg_set_version()]
#'
#' Reads back the version entries from a GeoPackage's metadata tables. Returns
#' `NULL` if the GeoPackage has no metadata table or no version entries -- so it
#' is safe to call on any GeoPackage.
#'
#' @param gpkg Path to a GeoPackage file.
#' @return A named `list` with `version` (semver string), `int_version`
#'   (integer), `license` (SPDX id, or `NULL`), and `provenance` (parsed JSON,
#'   or `NULL`); or `NULL` if no version metadata is present.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' info <- gpkg_get_version("hydrofabric.gpkg")
#' info$version
#' }
#' @export
gpkg_get_version <- function(gpkg) {
  stopifnot(file.exists(gpkg))
  prov_uri <- "https://lynker-spatial.com/ns/hydrofabric-provenance"
  spdx_uri <- "https://spdx.org/licenses/"

  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!"gpkg_metadata" %in% DBI::dbListTables(con)) return(NULL)

  q <- DBI::dbGetQuery(con,
    "SELECT md_standard_uri, metadata FROM gpkg_metadata WHERE md_standard_uri IN
       ('https://schema.org','https://semver.org', ?, ?)", params = list(prov_uri, spdx_uri))
  if (!nrow(q)) return(NULL)

  pick <- function(uri) {
    v <- q$metadata[q$md_standard_uri == uri]
    if (length(v)) v[[1]] else NA_character_
  }
  prov <- pick(prov_uri)
  lic  <- pick(spdx_uri)
  list(
    version     = pick("https://semver.org"),
    int_version = suppressWarnings(as.integer(pick("https://schema.org"))),
    license     = if (is.na(lic)) NULL else lic,
    provenance  = if (!is.na(prov)) jsonlite::fromJSON(prov) else NULL)
}

# ---- GeoPackage in-place write primitives -----------------------------------
# GeoPackage RTree/metadata triggers invoke SpatiaLite functions (ST_IsEmpty,
# ST_MinX, ...) that are unavailable under the RSQLite driver, so any UPDATE/
# DELETE on a spatial table aborts with "no such function". These helpers save,
# drop, and restore a layer's triggers around every DML operation.

.gpkg_save_drop_triggers <- function(con, layer) {
  trigs <- DBI::dbGetQuery(con,
    sprintf("SELECT name, sql FROM sqlite_master WHERE type='trigger' AND tbl_name='%s'",
      layer))
  for (nm in trigs$name)
    DBI::dbExecute(con, sprintf('DROP TRIGGER IF EXISTS "%s"', nm))
  trigs
}

.gpkg_restore_triggers <- function(con, trigs) {
  for (tsql in trigs$sql)
    if (!is.na(tsql) && nzchar(tsql)) DBI::dbExecute(con, tsql)
}

# Fully remove a layer written by sf/GDAL. Dropping the feature table alone is
# not enough: a spatial layer also owns an R-tree index (four `rtree_<layer>_
# <geom>*` shadow tables plus their triggers) and rows in the GeoPackage
# catalog tables. Left behind, those accumulate in the file on every call.
# GLOB rather than LIKE because `_` is a single-character wildcard in LIKE.
.gpkg_drop_layer <- function(con, layer) {
  trg <- DBI::dbGetQuery(con, sprintf(
    "SELECT name FROM sqlite_master WHERE type='trigger'
       AND (tbl_name = '%s' OR name GLOB 'rtree_%s_*')", layer, layer))$name
  for (nm in trg) DBI::dbExecute(con, sprintf('DROP TRIGGER IF EXISTS "%s"', nm))

  shadow <- DBI::dbGetQuery(con, sprintf(
    "SELECT name FROM sqlite_master WHERE type='table' AND name GLOB 'rtree_%s_*'",
    layer))$name
  for (nm in shadow) DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', nm))

  for (cat_tbl in c("gpkg_contents", "gpkg_geometry_columns",
                    "gpkg_extensions", "gpkg_ogr_contents")) {
    if (DBI::dbExistsTable(con, cat_tbl))
      DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE table_name = '%s'",
        cat_tbl, layer))
  }
  DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', layer))
  invisible(NULL)
}

#' In-place update of a single non-geometry column in a GeoPackage layer
#'
#' Updates `col` for the rows whose `id_col` matches `id_vals`, via a temporary
#' join table. Layer triggers are dropped for the write and restored afterward
#' (they call SpatiaLite functions unavailable under RSQLite, which would abort
#' the UPDATE). `id_vals` and `col_vals` are parallel vectors of equal length.
#'
#' @param gpkg Path to the GeoPackage.
#' @param layer Target layer (table) name.
#' @param id_col Name of the id column to match on.
#' @param id_vals Ids identifying the rows to update.
#' @param col Name of the (non-geometry) column to update.
#' @param col_vals New values for `col`, parallel to `id_vals`.
#' @return Invisibly `NULL`.
#' @export
gpkg_update_col <- function(gpkg, layer, id_col, id_vals, col, col_vals) {
  upd <- data.frame(._id  = as.character(id_vals),
    ._val = as.character(col_vals),
    stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "._upd", upd, temporary = TRUE, overwrite = TRUE)
  sql <- sprintf(
    paste0(
      'UPDATE "%s" SET "%s" = (SELECT "._val" FROM "._upd" ',
      'WHERE CAST("._upd"."._id" AS TEXT) = CAST("%s"."%s" AS TEXT)) ',
      'WHERE CAST("%s" AS TEXT) IN (SELECT "._id" FROM "._upd")'
    ),
    layer, col, layer, id_col, id_col)
  DBI::dbBegin(con)
  tryCatch(
    {
      trigs <- .gpkg_save_drop_triggers(con, layer)
      DBI::dbExecute(con, sql)
      .gpkg_restore_triggers(con, trigs)
      DBI::dbCommit(con)
    },
    error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    })
  invisible(NULL)
}

#' In-place geometry update for specific rows of a GeoPackage layer
#'
#' Writes the changed features to a temporary layer in the same GeoPackage, swaps
#' the geometry column into `layer` via a keyed (indexed) SQL UPDATE, then drops
#' the temp layer. Layer triggers are dropped/restored around the write (see
#' [gpkg_update_col()]).
#'
#' @details
#' Removing the temporary layer means removing everything GDAL created with it:
#' the feature table, its four `rtree_*` index shadow tables and their triggers,
#' and its rows in `gpkg_contents`, `gpkg_geometry_columns`, `gpkg_extensions`,
#' and `gpkg_ogr_contents`. All of it happens inside the same transaction as the
#' geometry swap, so a failure leaves the GeoPackage exactly as it was found.
#'
#' @param gpkg Path to the GeoPackage.
#' @param layer Target layer name.
#' @param id_col Id column used to match changed rows.
#' @param sf_changed An `sf` of the changed features (must carry `id_col`).
#' @return Invisibly `NULL`.
#' @export
gpkg_update_geom <- function(gpkg, layer, id_col, sf_changed) {
  # A clock-derived name collides when two calls land in the same centisecond,
  # and %OS2 puts a "." in the identifier; tempfile() gives a unique, plain
  # token without disturbing the caller's RNG state.
  tmp_lyr <- paste0("geom_upd_", basename(tempfile("")))
  sf::st_write(sf_changed, gpkg, tmp_lyr, append = FALSE, quiet = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  geom_main <- DBI::dbGetQuery(con,
    sprintf("SELECT column_name FROM gpkg_geometry_columns WHERE table_name = '%s'", layer))[[1]]
  geom_tmp <- DBI::dbGetQuery(con,
    sprintf("SELECT column_name FROM gpkg_geometry_columns WHERE table_name = '%s'", tmp_lyr))[[1]]
  DBI::dbBegin(con)
  tryCatch(
    {
      DBI::dbExecute(con, sprintf(
        'CREATE INDEX "idx_%s" ON "%s" (CAST("%s" AS TEXT))', tmp_lyr, tmp_lyr, id_col))
      trigs <- .gpkg_save_drop_triggers(con, layer)
      DBI::dbExecute(con, sprintf(
        paste0(
          'UPDATE "%s" SET "%s" = (SELECT "%s" FROM "%s" ',
          'WHERE CAST("%s"."%s" AS TEXT) = CAST("%s"."%s" AS TEXT)) ',
          'WHERE CAST("%s" AS TEXT) IN (SELECT CAST("%s" AS TEXT) FROM "%s")'
        ),
        layer, geom_main, geom_tmp, tmp_lyr, tmp_lyr, id_col, layer, id_col,
        id_col, id_col, tmp_lyr))
      .gpkg_restore_triggers(con, trigs)
      .gpkg_drop_layer(con, tmp_lyr)
      DBI::dbCommit(con)
    },
    error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    })
  invisible(NULL)
}

#' Run arbitrary SQL against a GeoPackage in a single transaction
#'
#' Each `...` item is either a SQL string (executed in order), a
#' `list(table=, df=)` (written as a temporary table first), or a
#' `list(drop_triggers_for=)` (drops that layer's triggers, restored at commit).
#' All statements run in one transaction; any error rolls back.
#'
#' @section Updating a spatial layer:
#' Any `UPDATE` against a layer that has a geometry column needs a
#' `list(drop_triggers_for = <layer>)` ahead of it, including when the
#' statement touches only attribute columns. GDAL's R-tree triggers call
#' SpatiaLite functions such as `ST_IsEmpty()` that RSQLite does not provide,
#' so the write fails with `no such function: ST_IsEmpty` and the transaction
#' rolls back. A temporary table staged with `list(table=, df=)` must be
#' referenced with a quoted identifier (`"._patch"`) if its name starts with a
#' character SQLite would otherwise parse, such as a dot.
#'
#' @param gpkg Path to the GeoPackage.
#' @param ... SQL strings and/or the list forms described above.
#' @return Invisibly `NULL`.
#' @export
gpkg_exec <- function(gpkg, ...) {
  items  <- list(...)
  con    <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  saved_trigs <- list()
  DBI::dbBegin(con)
  tryCatch(
    {
      for (item in items) {
        if (is.character(item)) {
          DBI::dbExecute(con, item)
        } else if (!is.null(item[["drop_triggers_for"]])) {
          lyr <- item[["drop_triggers_for"]]
          if (is.null(saved_trigs[[lyr]]))
            saved_trigs[[lyr]] <- .gpkg_save_drop_triggers(con, lyr)
        } else {
          DBI::dbWriteTable(con, item[["table"]], item[["df"]],
            temporary = TRUE, overwrite = TRUE)
        }
      }
      for (trigs in saved_trigs) .gpkg_restore_triggers(con, trigs)
      DBI::dbCommit(con)
    },
    error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    })
  invisible(NULL)
}
