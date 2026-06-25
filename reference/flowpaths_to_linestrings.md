# Convert MULTILINESTRINGS to LINESTRINGS

Convert MULTILINESTRINGS to LINESTRINGS

## Usage

``` r
flowpaths_to_linestrings(flowpaths)
```

## Arguments

- flowpaths:

  a flowpath \`sf\` object

## Value

a \`sf\` object

## Examples

``` r
if (FALSE) { # \dontrun{
fl <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
fl <- flowpaths_to_linestrings(fl)
} # }
```
