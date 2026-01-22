#' Authenticate with Lynker Spatial
#' @param token An existing OAuth2 token. If NULL, then a new token is provisioned and returned.
#'              If the token is expired, then
#' @param ... Unused
#' @param libs Supported libraries to configure auth for.
#' @param duckdb_con A DuckDB DBI connection to add a bearer token secret to.
#' @returns The `token` argument, or a newly provisioned token
#'
#' @details
#' lynker_spatial.auth.token
#' 
#' @export
lynker_spatial_auth <- function(
  token = getOption("lynker_spatial.token"),
  ...,
  libs = c("gdal", "duckdb"),
  duckdb_con = NULL
) {
  if (is.null(token)) {
    token <- lynker_spatial_token()
  }

  libs <- match.arg(libs, several.ok = TRUE)

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

  # TODO: Set Arrow Bearer (need filesystem impl)
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
#' @keywords internal
lynker_spatial_token <- function(..., client = lynker_spatial_client()) {
  # Get the token using the OIDC client
  token <- httr2::oauth_flow_auth_code(
    client,
    auth_url = client$provider$authorization_endpoint,
    scope = "openid profile email phone",
    redirect_uri = client$redirect_uri,
    pkce = TRUE
  )

  options("lynker_spatial.token" = token)
  token
}

#' Refresh an existing Lynker Spatial token
#' @keywords internal
lynker_spatial_refresh <- function(token = getOption("lynker_spatial.token"), ..., client = lynker_spatial_client()) {
  if (inherits(token, "httr2_token")) {
    refresh_token <- token$refresh_token
  } else if (is.character(token)) {
    refresh_token <- token
  } else {
    stop("token is malformed", call. = FALSE)
  }

  httr2::oauth_flow_refresh(client, token$refresh_token, scope = "openid profile email phone")
}
