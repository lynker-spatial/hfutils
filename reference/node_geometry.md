# Node geometry from line endpoints

Replaces the geometry column of an \`sf\` object with point geometries
taken from each feature's line endpoints (or start nodes). Thin wrapper
around \[get_node()\].

## Usage

``` r
node_geometry(x, position = "end")
```

## Arguments

- x:

  An \`sf\` object (typically LINESTRING/MULTILINESTRING).

- position:

  Character string, either \`"end"\` (default) or \`"start"\`, forwarded
  to \[get_node()\] to choose which node to extract.

## Value

An \`sf\` object with geometry set to the requested node locations.

## See also

\[get_node()\]

## Examples

``` r
if (FALSE) { # \dontrun{
pts <- node_geometry(flow_sf, position = "end")
} # }
```
