# Read Parquet Dataset

Read an Arrow multi-file dataset and create `sf` object

## Usage

``` r
read_sf_dataset(dataset, find_geom = FALSE)
```

## Arguments

- dataset:

  a `Dataset` object created by
  [`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  or an `arrow_dplyr_query`

- find_geom:

  logical. Only needed when returning a subset of columns. Should all
  available geometry columns be selected and added to to the dataset
  query without being named? Default is `FALSE` to require geometry
  column(s) to be selected specifically.

## Value

object of class [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Details

This function is primarily for use after opening a dataset with
[`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html).
Users can then query the `arrow Dataset` using `dplyr` methods such as
[`filter`](https://dplyr.tidyverse.org/reference/filter.html) or
[`select`](https://dplyr.tidyverse.org/reference/select.html). Passing
the resulting query to this function will parse the datasets and create
an `sf` object. The function expects consistent geographic metadata to
be stored with the dataset in order to create
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) objects.
Adopted from [wcjochem/sfarrow](https://github.com/wcjochem/sfarrow)

## See also

[`open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html),
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_read_parquet`](st_read_parquet.md)
