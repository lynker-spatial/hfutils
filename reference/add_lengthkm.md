# Compute length in kilometers (numeric)

Safely compute linestring length in km and return a plain numeric
vector.

## Usage

``` r
add_lengthkm(x)
```

## Arguments

- x:

  An \`sf\` object with LINE\* geometry. If \`x\` is not in a projected
  CRS, \`sf::st_length()\` will compute ellipsoidal lengths when
  possible.

## Value

A numeric vector of lengths in kilometers.

## Examples

``` r
if (FALSE) { # \dontrun{
fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
fl$lengthkm <- add_lengthkm(fl)
} # }
```
