# Fast polygon union by ID

Significantly faster than \`sf::st_union()\`/\`dplyr::summarise()\` for
unioning large polygon datasets by a grouping column, by leveraging
terra’s \`aggregate()\` with a round-trip through \`terra::vect()\`.

## Usage

``` r
union_polygons(poly, ID)
```

## Arguments

- poly:

  An \`sf\` POLYGON/MULTIPOLYGON object with an attribute column used
  for grouping.

- ID:

  A string naming the column over which to union geometries.

## Value

An \`sf\` polygon layer unioned by \`ID\` (column preserved).

## Notes

If any resulting geometries are geometry collections, they are extracted
to POLYGON using \`sf::st_collection_extract()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
out <- union_polygons(counties_sf, "state_fips")
} # }
```
