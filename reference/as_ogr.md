# Delayed read for vector resources

A lazy data frame for GDAL vector data sources. as_ogr is DBI compatible
and designed to work with dplyr.

## Usage

``` r
as_ogr(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs)

# S3 method for class 'character'
as_ogr(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs)

# S3 method for class 'OGRSQLConnection'
as_ogr(x, layer, ..., query = NA, ignore_lyrs = .hf_ignore_lyrs)
```

## Arguments

- x:

  the data source (file path, url, or database connection)

- layer:

  layer name (varies by driver, may be a file name without extension);
  in case `layer` is missing, `st_read` will read the first layer of
  `dsn`, give a warning and (unless `quiet = TRUE`) print a message when
  there are multiple layers, or give an error if there are no layers in
  `dsn`. If `dsn` is a database connection, then `layer` can be a table
  name or a database identifier (see
  [`Id`](https://dbi.r-dbi.org/reference/Id.html)). It is also possible
  to omit `layer` and rather use the `query` argument.

- ...:

  parameter(s) passed on to
  [st_as_sf](https://r-spatial.github.io/sf/reference/st_as_sf.html)

- query:

  SQL query to pass in directly

- ignore_lyrs:

  Regular expression matching tables that are data-source plumbing
  rather than user layers, excluded when \`layer\` is not given. The
  default drops GeoPackage spec tables (\`^gpkg\_\`), spatial indexes
  (\`^rtree\_\`), SQLite internals (\`^sqlite\_\`), and the two tables
  QGIS writes into a GeoPackage when you save a style or a project to it
  (\`layer_styles\`, \`qgis_projects\`). Patterns are anchored, so a
  layer whose name merely contains one of these is not dropped.

## Value

a 'tbl_OGRSQLConnection'

## Details

The output of \`as_ogr()\` is a 'tbl_OGRSQLConnection\` that extends
\`tbl_dbi\` and may be used with functions and workflows in the normal
DBI way, see \[OGRSQL()\] for the as_ogr DBI support.

To obtain an in memory data frame use an explicit \`collect()\` or
\`st_as_sf()\`. A call to \`collect()\` is triggered by \`st_as_sf()\`
and will add the sf class to the output.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
tbl <- as_ogr("hydrofabric.gpkg", "flowpaths")
tbl |> filter(order >= 4) |> st_as_sf()
} # }
```
