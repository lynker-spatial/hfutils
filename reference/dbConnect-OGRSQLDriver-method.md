# dbConnect

dbConnect for sources that can be read by package sf

## Usage

``` r
# S4 method for class 'OGRSQLDriver'
dbConnect(drv, DSN = "", readonly = TRUE, ...)
```

## Arguments

- drv:

  OGRSQLDriver created by [`OGRSQL()`](OGRSQL.md)

- DSN:

  data source name

- readonly:

  open in readonly mode (\`TRUE\` is the only option)

- ...:

  ignored

## Details

The 'OGRSQL' available is documented with GDAL:
https://gdal.org/user/ogr_sql_dialect.html
