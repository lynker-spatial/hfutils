# Compute Pfafstetter basin codes over a directed acyclic network

Assigns hierarchical Pfafstetter codes (the NHDPlus basin-coding
scheme). At each level the basin's mainstem is found, its four largest
tributaries (by total drainage area) are given even digits \`2,4,6,8\`
ordered downstream to upstream, and the five mainstem inter-basins
between those junctions take odd digits \`1,3,5,7,9\`. Each of the nine
sub-basins is then subdivided the same way, appending a digit per level,
down to \`max_level\`.

## Usage

``` r
get_pfafstetter(
  x,
  id = "flowpath_id",
  toid = "flowpath_toid",
  total_da = "total_da_sqkm",
  topo_sort = "topo_sort",
  levelpath = "levelpath",
  max_level = 2
)
```

## Arguments

- x:

  A data frame with \`id\`, downstream pointer \`toid\`, \`total_da\`
  (total upstream drainage area), \`topo_sort\` (a hydrosequence;
  smaller is more downstream, e.g. from \[get_hydroseq()\]), and
  \`levelpath\` (e.g. from \[get_levelpath()\]). Terminal/outlet rows
  use \`NA\`, \`""\`, \`"0"\`, or an unknown \`toid\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

- total_da, topo_sort, levelpath:

  Column names for total drainage area, hydrosequence, and level-path
  id. Defaults \`"total_da_sqkm"\`, \`"topo_sort"\`, \`"levelpath"\`.

- max_level:

  Integer. Number of Pfafstetter levels (digits) to assign. Default
  \`2\`.

## Value

Numeric vector of \`max_level\`-digit Pfafstetter codes aligned to the
rows of \`x\`. Reaches whose sub-basin is deeper than \`max_level\`
levels are \`NA\`.

## Details

Requires the drainage-area, hydrosequence, and level-path columns to be
precomputed (see \[accumulate_downstream()\], \[get_hydroseq()\],
\[get_levelpath()\]); this keeps the coding independent of how those
were derived. Ties in the four-largest-tributary cut are resolved by
keeping all tied tributaries.

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
if (FALSE) { # \dontrun{
x$total_da_sqkm <- accumulate_downstream(x, attr = "areasqkm")
x$topo_sort     <- get_hydroseq(x)
x$levelpath     <- get_levelpath(x, weight = "total_da_sqkm")
x$pfaf          <- get_pfafstetter(x, max_level = 2)
} # }
```
