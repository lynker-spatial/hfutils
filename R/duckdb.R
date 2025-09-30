#' Create a new DuckDB connection.
#' @param ... Arguments passed to [DBI::dbConnect()].
#' @param extensions Character vector of extensions to install and load on connect.
#' @param add_auth Include Lynker Spatial authentication.
#' @returns A DBI connection to a DuckDB instance.
#' @export
duckdb_connection <- function(..., extensions = character(0), add_auth = TRUE) {
  conn <- DBI::dbConnect(duckdb::duckdb(), ...)

  if (length(extensions) > 0) {
    for (ext in extensions) {
      DBI::dbExecute(conn, paste("INSTALL", ext))
      DBI::dbExecute(conn, paste("LOAD", ext))
    }
  }

  if (add_auth) {
    lynker_spatial_auth(libs = c("gdal", "duckdb"), duckdb_con = conn)
  }

  conn
}

#' Read DuckDB File(s) over HTTP
#' @param urls 1 or more URLs
#' @param ... Unused
#' @param conn A DuckDB connection
#' @param read_func The DuckDB SQL function to call against the list of urls.
#'                  Defaults to `read_parquet`.
#' @export
tbl_http <- function(
  urls,
  ...,
  conn = duckdb_connection(extensions = "httpfs"),
  read_func = c("read_parquet", "read_csv", )
) {

  # TODO(justin): allow read_func arguments i.e. union_by_name
  query <- paste0("SELECT * FROM ", read_func, "([",
    paste0("'", urls, "'", collapse = ","),
  "])")

  dplyr::tbl(conn, dbplyr::sql(query))
}
