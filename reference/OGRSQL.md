# OGRSQL OGRSQL driver, use to \[dbConnect()\] to a data source readable by sf

OGRSQL OGRSQL driver, use to \[dbConnect()\] to a data source readable
by sf

## Usage

``` r
OGRSQL()
```

## Examples

``` r
if (FALSE) { # \dontrun{
con <- dbConnect(OGRSQL(), "hydrofabric.gpkg")
as_ogr(con, "flowpaths")
} # }
```
