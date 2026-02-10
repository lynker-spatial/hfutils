# Authenticate with Lynker Spatial

Authenticate with Lynker Spatial

## Usage

``` r
lynker_spatial_auth(
  token = NULL,
  ...,
  libs = c("gdal", "duckdb"),
  duckdb_con = NULL
)
```

## Arguments

- token:

  An existing OAuth2 token. If NULL, then a new token is provisioned and
  returned. If the token is expired, then

- ...:

  Unused

- libs:

  Supported libraries to configure auth for.

- duckdb_con:

  A DuckDB DBI connection to add a bearer token secret to.

## Value

The \`token\` argument, or a newly provisioned token
