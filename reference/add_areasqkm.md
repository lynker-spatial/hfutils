# Compute area in square kilometers (numeric)

Safely compute polygon area in km^2 and return a plain numeric vector.

## Usage

``` r
add_areasqkm(x)
```

## Arguments

- x:

  An \`sf\` object with polygonal geometry. If \`x\` is not in a
  projected CRS, \`sf::st_area()\` will compute ellipsoidal areas when
  possible.

## Value

A numeric vector of areas in square kilometers.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
add_areasqkm(nc[1, ])
} # }
```
