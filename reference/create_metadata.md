# Create standardized geo metadata for Parquet files

Create standardized geo metadata for Parquet files

## Usage

``` r
create_metadata(
  df,
  hf_version = "2.2",
  license = "CC-BY-NC-SA",
  source = "lynker-spatial"
)
```

## Arguments

- df:

  object of class `sf`

- hf_version:

  dataset version

- license:

  dataset license

- source:

  dataset source

## Value

JSON formatted list with geo-metadata

## Details

Reference for metadata standard:
<https://github.com/geopandas/geo-arrow-spec>. This is compatible with
`GeoPandas` Parquet files. Adopted from
[wcjochem/sfarrow](https://github.com/wcjochem/sfarrow)
