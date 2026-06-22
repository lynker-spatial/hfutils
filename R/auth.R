#' Authenticate with Lynker Spatial
#' @param token An existing OAuth2 token. If NULL, then a new token is provisioned and returned.
#'              If the token is expired, then
#' @param ... Unused
#' @param libs Libraries to configure auth for; any of `"gdal"`, `"duckdb"`,
#'   `"arrow"`. Default `c("gdal", "duckdb")`. `"arrow"` authenticates via the
#'   AWS S3 credential chain rather than the bearer token (see Details).
#' @param duckdb_con A DuckDB DBI connection to add a bearer token secret to.
#' @details Per library: `"gdal"` sets the `GDAL_HTTP_AUTH` /
#'   `GDAL_HTTP_BEARER` environment variables; `"duckdb"` creates a
#'   bearer-token HTTP secret on `duckdb_con`. `"arrow"` does *not* use the
#'   bearer -- the arrow R bindings expose no HTTP custom-header filesystem, so
#'   authenticated arrow access is via S3 (`arrow::S3FileSystem`, credentials
#'   from the standard AWS chain); requesting `"arrow"` only warns when arrow
#'   lacks S3 support.
#' @returns The `token` argument, or a newly provisioned token
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(duckdb::duckdb())
#' token <- lynker_spatial_auth(libs = c("gdal", "duckdb"), duckdb_con = conn)
#' }
#' @export
lynker_spatial_auth <- function(
  token = NULL,
  ...,
  libs = c("gdal", "duckdb"),
  duckdb_con = NULL
) {
  if (is.null(token)) {
    token <- lynker_spatial_token()
  }

  libs <- match.arg(libs, c("gdal", "duckdb", "arrow"), several.ok = TRUE)

  if (inherits(token, "httr2_token")) {
    # if (now > expired_time)
    if ("expires_at" %in% names(token) && Sys.time() >= as.POSIXct(token$expires_at)) {
      token <- lynker_spatial_refresh(token)
    }

    id_token <- token$id_token
  } else {
    stop("token is malformed", call. = FALSE)
  }

  # Set GDAL Bearer
  if ("gdal" %in% libs) {
    Sys.setenv(GDAL_HTTP_AUTH = "BEARER", GDAL_HTTP_BEARER = id_token)
  }

  # Set DuckDB Bearer
  if ("duckdb" %in% libs && !is.null(duckdb_con)) {
    DBI::dbExecute(duckdb_con, sprintf(
      "CREATE OR REPLACE SECRET lynker_spatial_auth (TYPE http, BEARER_TOKEN '%s')",
      id_token
    ))
  }

  # Arrow: the R arrow bindings (<= 23.x) expose only Local/S3/Gcs filesystems
  # with no HTTP custom-header support, so the OAuth id_token cannot be attached
  # to arrow's HTTPS reads. Arrow instead authenticates lynker-spatial via S3 --
  # arrow::S3FileSystem resolves credentials from the standard AWS chain
  # (env / shared config / SSO), so `s3://` reads are already authenticated when
  # AWS credentials are present and there is no bearer to set. Protected
  # HTTPS-gateway reads should use the DuckDB (`tbl_http()`) or GDAL path above.
  if ("arrow" %in% libs &&
      (!requireNamespace("arrow", quietly = TRUE) || !arrow::arrow_with_s3())) {
    warning("arrow is built without S3 support; authenticated lynker-spatial ",
            "arrow reads are unavailable -- use the DuckDB/GDAL path for ",
            "protected data.", call. = FALSE)
  }

  token
}

#' Get an OAuth2 Client for Lynker Spatial
#' @keywords internal
lynker_spatial_client <- function() {
  provider <-
    httr2::request("https://cognito-idp.us-west-2.amazonaws.com/us-west-2_em0hAPqnS") |>
    httr2::req_url_path_append(".well-known", "openid-configuration") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    structure(class = c("oidc_provider", "list"))

  # Don't need secret since we are using an app client for
  # a public-facing application.
  client <- httr2::oauth_client(
    id = "1he6ti5109b9t6r1ifd4brecpl",
    token_url = provider$token_endpoint,
    auth = "body"
  )

  # NOTE(justin): we need to set this to have a static port
  #               because the callback URI also needs to be
  #               set in the IdP.
  client[["redirect_uri"]] <- "http://localhost:57777"
  client[["provider"]] <- provider

  # This is primarily for visuals if the client is printed
  class(client) <- c("oidc_client", class(client))
  client
}

#' Get an OAuth2 token for Lynker Spatial
#' @param ... Unused; reserved for future options.
#' @param client An OIDC client, as returned by [lynker_spatial_client()].
#' @return An httr2 OAuth2 token.
#' @examples
#' \dontrun{
#' token <- lynker_spatial_token()
#' }
#' @export
lynker_spatial_token <- function(..., client = lynker_spatial_client()) {
  # Get the token using the OIDC client
  httr2::oauth_flow_auth_code(
    client,
    auth_url = client$provider$authorization_endpoint,
    scope = "openid profile email phone",
    redirect_uri = client$redirect_uri,
    pkce = TRUE
  )
}

#' Refresh an existing Lynker Spatial token
#' @param token An existing httr2 OAuth2 token to refresh.
#' @param ... Unused; reserved for future options.
#' @param client An OIDC client, as returned by [lynker_spatial_client()].
#' @return A refreshed httr2 OAuth2 token.
#' @examples
#' \dontrun{
#' token <- lynker_spatial_token()
#' token <- lynker_spatial_refresh(token)
#' }
#' @export
lynker_spatial_refresh <- function(token, ..., client = lynker_spatial_client()) {
  if (!inherits(token, "httr2_token")) {
    stop("token is malformed", call. = FALSE)
  }

  httr2::oauth_flow_refresh(client, token$refresh_token, scope = "openid profile email phone")
}
