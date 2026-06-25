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

  An existing OAuth2 token. If \`NULL\`, a new token is provisioned and
  returned. If the supplied token is expired, it is refreshed via
  \[lynker_spatial_refresh()\] before use.

- ...:

  Unused

- libs:

  Libraries to configure auth for; any of \`"gdal"\`, \`"duckdb"\`,
  \`"arrow"\`. Default \`c("gdal", "duckdb")\`. \`"arrow"\`
  authenticates via the AWS S3 credential chain rather than the bearer
  token (see Details).

- duckdb_con:

  A DuckDB DBI connection to add a bearer token secret to.

## Value

The \`token\` argument, or a newly provisioned token

## Details

Per library: \`"gdal"\` sets the \`GDAL_HTTP_AUTH\` /
\`GDAL_HTTP_BEARER\` environment variables; \`"duckdb"\` creates a
bearer-token HTTP secret on \`duckdb_con\`. \`"arrow"\` does \*not\* use
the bearer – the arrow R bindings expose no HTTP custom-header
filesystem, so authenticated arrow access is via S3
(\`arrow::S3FileSystem\`, credentials from the standard AWS chain);
requesting \`"arrow"\` only warns when arrow lacks S3 support.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- DBI::dbConnect(duckdb::duckdb())
token <- lynker_spatial_auth(libs = c("gdal", "duckdb"), duckdb_con = conn)
} # }
```
