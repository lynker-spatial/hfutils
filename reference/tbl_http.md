# Read DuckDB File(s) over HTTP

Read DuckDB File(s) over HTTP

## Usage

``` r
tbl_http(
  urls,
  ...,
  conn = duckdb_connection(extensions = "httpfs"),
  read_func = c("read_parquet", "read_csv", )
)
```

## Arguments

- urls:

  1 or more URLs

- ...:

  Unused

- conn:

  A DuckDB connection

- read_func:

  The DuckDB SQL function to call against the list of urls. Defaults to
  \`read_parquet\`.
