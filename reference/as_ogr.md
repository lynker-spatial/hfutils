# Delayed read for vector resources

A lazy data frame for GDAL vector data sources. as_ogr is DBI compatible
and designed to work with dplyr.

## Usage

``` r
as_ogr(x, layer, ..., query = NA, ignore_lyrs = "gpkg_|rtree_|sqlite_")

# S3 method for class 'character'
as_ogr(x, layer, ..., query = NA, ignore_lyrs = "gpkg_|rtree_|sqlite_")

# S3 method for class 'OGRSQLConnection'
as_ogr(x, layer, ..., query = NA, ignore_lyrs = "gpkg_|rtree_|sqlite_")
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

  pattern for layers to be ignored description

## Value

a 'tbl_OGRSQLConnection'

## Details

The output of \`as_ogr()\` is a 'tbl_OGRSQLConnection\` that extends
\`tbl_dbi\` and may be used with functions and workflows in the normal
DBI way, see \[OGRSQL()\] for the as_ogr DBI support.

To obtain an in memory data frame use an explict \`collect()\` or
\`st_as_sf()\`. A call to \`collect()\` is triggered by \`st_as_sf()\`
and will add the sf class to the output.
