# Node geometry from line endpoints

Replaces the geometry column of an \`sf\` object with point geometries
taken from each feature's line endpoints (or start nodes). This is a
thin wrapper around an internal helper \`.get_node()\` that extracts the
desired node from a geometry vector.

## Usage

``` r
node_geometry(x, position = "end")
```

## Arguments

- x:

  An \`sf\` object (typically LINESTRING/MULTILINESTRING).

- position:

  Character string, either \`"end"\` (default) or \`"start"\`, forwarded
  to \`.get_node()\` to choose which node to extract.

## Value

An \`sf\` object with geometry set to the requested node locations.

## Details

This function requires an internal helper \`.get_node(geom, position)\`
that returns an \`sfc\` of points given an \`sfc\` of line geometries
and a position of \`"start"\` or \`"end"\`. Make sure that helper exists
in your package.

## Examples

``` r
if (FALSE) { # \dontrun{
pts <- node_geometry(flow_sf, position = "end")
} # }
```
