# Create a new DuckDB connection.

Create a new DuckDB connection.

## Usage

``` r
duckdb_connection(..., extensions = character(0), add_auth = TRUE)
```

## Arguments

- ...:

  Arguments passed to \[DBI::dbConnect()\].

- extensions:

  Character vector of extensions to install and load on connect.

- add_auth:

  Include Lynker Spatial authentication.

## Value

A DBI connection to a DuckDB instance.
