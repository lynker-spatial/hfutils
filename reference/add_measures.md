# Add length and area measures to flowpaths/divides

Add length and area measures to flowpaths/divides

## Usage

``` r
add_measures(flowpaths, divides)
```

## Arguments

- flowpaths:

  sf LINESTRING

- divides:

  sf POLYGON

## Value

named list of updated flowpaths and divides

## Examples

``` r
if (FALSE) { # \dontrun{
fps <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
divs <- sf::read_sf("hydrofabric.gpkg", "divides")
out <- add_measures(fps, divs)
out$flowpaths$lengthkm
} # }
```
