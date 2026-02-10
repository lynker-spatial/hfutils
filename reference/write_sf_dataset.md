# Write Parquet Dataset

Write `sf` object to an Arrow multi-file dataset

## Usage

``` r
write_sf_dataset(
  obj,
  path,
  format = "parquet",
  partitioning = dplyr::group_vars(obj),
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

- path:

  string path referencing a directory for the output

- format:

  output file format ("parquet" or "feather")

- partitioning:

  character vector of columns in `obj` for grouping or the
  [`dplyr::group_vars`](https://dplyr.tidyverse.org/reference/group_data.html)

- hf_version:

  dataset version

- license:

  dataset license

- source:

  dataset source

- ...:

  additional arguments and options passed to
  [`arrow::write_dataset`](https://arrow.apache.org/docs/r/reference/write_dataset.html)

## Value

`obj` invisibly

## Details

Translate an `sf` spatial object to `data.frame` with WKB geometry
columns and then write to an `arrow` dataset with partitioning. Allows
for `dplyr` grouped datasets (using
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)) and
uses those variables to define partitions. Adopted from
[wcjochem/sfarrow](https://github.com/wcjochem/sfarrow)

## See also

[`write_dataset`](https://arrow.apache.org/docs/r/reference/write_dataset.html),
[`st_read_parquet`](st_read_parquet.md)
