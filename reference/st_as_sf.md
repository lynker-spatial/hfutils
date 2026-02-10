# Force collection of a OGR query Convert as_ogr to a data frame or sf object

Force collection of a OGR query Convert as_ogr to a data frame or sf
object

## Usage

``` r
# S3 method for class 'tbl_OGRSQLConnection'
st_as_sf(x, ...)
```

## Arguments

- x:

  output of \[as_ogr()\]

- ...:

  passed to \[collect()\]

## Value

a data frame from \`collect()\`, sf data frame from \`st_as_sf()\` (only
if it contains an \`sfc\` geometry column)
