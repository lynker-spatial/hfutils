# Rename geometry column of sf object

Rename geometry column of sf object

## Usage

``` r
rename_geometry(g, name = "geometry")
```

## Arguments

- g:

  sf object

- name:

  new geometry name. Default \`"geometry"\`.

## Value

sf object with renamed geometry

## Examples

``` r
if (FALSE) { # \dontrun{
fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
fl <- rename_geometry(fl, "geometry")
} # }
```
