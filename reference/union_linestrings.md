# Fast linestring union by ID

Fast union/merge of lines by grouping column, using \`terra\`'s
\`aggregate()\` and returning an \`sf\` layer. Final conversion to clean
LINESTRINGs is delegated to an internal helper
\`flowpaths_to_linestrings()\`.

## Usage

``` r
union_linestrings(lines, ID)
```

## Arguments

- lines:

  An \`sf\` LINESTRING/MULTILINESTRING object with an attribute column
  used for grouping.

- ID:

  A string naming the column over which to union geometries.

## Value

An \`sf\` lines layer unioned by \`ID\` (column preserved). Output is
normalized to clean LINESTRINGs via \[flowpaths_to_linestrings()\].

## See also

\[flowpaths_to_linestrings()\], \[union_polygons()\]

## Examples

``` r
if (FALSE) { # \dontrun{
out <- union_linestrings(flow_sf, "group_id")
} # }
```
