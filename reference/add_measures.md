# Add length and area measures to flowpaths/divides

Computes \`lengthkm\` on flowpaths and \`areasqkm\` on divides, then
joins each divide's incremental \`areasqkm\` onto its flowpath. Any
\`areasqkm\` already on \`flowpaths\` is dropped first, so the returned
value always comes from \`divides\`. The geometry column of both layers
is renamed to \`"geometry"\`.

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

## Details

The divide-to-flowpath join prefers an explicit \`flowpath_id\` column
on \`divides\`, which the current schema requires because \`divide_id\`
and \`flowpath_id\` use different namespaces (\`cat-\*\` vs \`fp-\*\`).
When \`divides\` carries no \`flowpath_id\`, the join falls back to the
legacy 1:1 convention that \`divide_id == flowpath_id\`. Picking the
wrong key here matches nothing and silently zeroes \`areasqkm\`, along
with any total area accumulated from it, so a fabric whose divides lack
\`flowpath_id\` under the current schema should be repaired rather than
passed through this fallback.

## See also

\[add_lengthkm()\], \[add_areasqkm()\]

## Examples

``` r
if (FALSE) { # \dontrun{
fps <- sf::read_sf("hydrofabric.gpkg", "flowpaths")
divs <- sf::read_sf("hydrofabric.gpkg", "divides")
out <- add_measures(fps, divs)
out$flowpaths$lengthkm
} # }
```
