# Read DuckDB File(s) over HTTP

Read DuckDB File(s) over HTTP

## Usage

``` r
tbl_http(
  urls,
  ...,
  conn = duckdb_connection(extensions = "httpfs"),
  read_func = c("read_parquet", "read_csv")
)
```

## Arguments

- urls:

  1 or more URLs

- ...:

  Named options forwarded to the DuckDB reader function, e.g.
  \`union_by_name = TRUE\` becomes \`union_by_name=true\`. Logicals map
  to \`true\`/\`false\`, character values are single-quoted.

- conn:

  A DuckDB connection

- read_func:

  The DuckDB SQL function to call against the list of urls. One of
  \`"read_parquet"\` (default) or \`"read_csv"\`.

## Examples

``` r
if (FALSE) { # \dontrun{
urls <- "https://lynker-spatial.s3.amazonaws.com/v20.1/divides.parquet"
tbl_http(urls, union_by_name = TRUE) |>
  dplyr::filter(vpuid == "01") |>
  dplyr::collect()
} # }
```
