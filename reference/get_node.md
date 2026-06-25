# Get endpoint or startpoint of LINESTRING

Get endpoint or startpoint of LINESTRING

## Usage

``` r
get_node(x, position = "end")
```

## Arguments

- x:

  sf LINESTRING

- position:

  "start" or "end"

## Value

sf POINT

## Examples

``` r
if (FALSE) { # \dontrun{
fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
outlets <- get_node(sf::st_geometry(fl), position = "end")
} # }
```
