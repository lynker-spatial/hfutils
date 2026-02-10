# Encode Well Known Binary

Convert `sfc` geometry columns into a WKB binary format

## Usage

``` r
encode_wkb(df)
```

## Arguments

- df:

  `sf` object

## Value

`data.frame` with binary geometry column(s)

## Details

Allows for more than one geometry column in `sfc` format. Adopted from
[wcjochem/sfarrow](https://github.com/wcjochem/sfarrow)
