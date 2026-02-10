# Write `sf` object to Parquet file

Convert a simple features spatial object from `sf` to a Parquet file
using
[`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html).
Geometry columns (type `sfc`) are converted to well-known binary (WKB)
format.

## Usage

``` r
st_write_parquet(
  obj,
  dsn,
  hf_version = "2.2",
  license = "ODbL",
  source = "spatial.water.noaa.gov",
  ...
)
```

## Arguments

- obj:

  object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

- dsn:

  data source name. A path and file name with .parquet extension

- hf_version:

  dataset version

- license:

  dataset license

- source:

  dataset source

- ...:

  additional options to pass to
  [`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html)

## Value

`obj` invisibly

## Details

Adopted from [wcjochem/sfarrow](https://github.com/wcjochem/sfarrow)

## See also

[`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html)
