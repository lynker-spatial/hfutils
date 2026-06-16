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
#' @param scope GeoPackage metadata reference scope. Default `"geopackage"`
#'   (whole-dataset).
#'
#' @return The `gpkg` path, invisibly.
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite toJSON
#' @export
gpkg_set_version <- function(gpkg, version, int_version = semver_to_int(version),
                             provenance = NULL, scope = "geopackage") {

  stopifnot(file.exists(gpkg), length(version) == 1L, !is.na(version))

  prov_uri <- "https://lynker-spatial.com/ns/hydrofabric-provenance"
  uris     <- c("https://schema.org", "https://semver.org", prov_uri)

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

  invisible(gpkg)
}

#' Read the dataset version written by [gpkg_set_version()]
#'
#' Reads back the version entries from a GeoPackage's metadata tables. Returns
#' `NULL` if the GeoPackage has no metadata table or no version entries — so it
#' is safe to call on any GeoPackage.
#'
#' @param gpkg Path to a GeoPackage file.
#' @return A named `list` with `version` (semver string), `int_version`
#'   (integer), and `provenance` (parsed JSON, or `NULL`); or `NULL` if no
#'   version metadata is present.
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite fromJSON
#' @export
gpkg_get_version <- function(gpkg) {
  stopifnot(file.exists(gpkg))
  prov_uri <- "https://lynker-spatial.com/ns/hydrofabric-provenance"

  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!"gpkg_metadata" %in% DBI::dbListTables(con)) return(NULL)

  q <- DBI::dbGetQuery(con,
    "SELECT md_standard_uri, metadata FROM gpkg_metadata WHERE md_standard_uri IN
       ('https://schema.org','https://semver.org', ?)", params = list(prov_uri))
  if (!nrow(q)) return(NULL)

  pick <- function(uri) {
    v <- q$metadata[q$md_standard_uri == uri]
    if (length(v)) v[[1]] else NA_character_
  }
  prov <- pick(prov_uri)
  list(
    version     = pick("https://semver.org"),
    int_version = suppressWarnings(as.integer(pick("https://schema.org"))),
    provenance  = if (!is.na(prov)) jsonlite::fromJSON(prov) else NULL)
}
