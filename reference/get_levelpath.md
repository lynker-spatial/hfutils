# Compute mainstem level paths over a directed acyclic network

Same topological approach as \[get_hydroseq()\] / \[get_streamorder()\]
(igraph topo-sort). A level path is a continuous mainstem from a
headwater to an outlet: at each confluence the mainstem continues up the
contributor with the largest \`weight\` (e.g. arbolate sum or total
drainage area), and the other contributors begin new level paths. The id
of a level path is the hydrosequence of its most-downstream (outlet)
reach, matching the NHDPlus convention.

## Usage

``` r
get_levelpath(
  x,
  id = "flowpath_id",
  toid = "flowpath_toid",
  weight,
  hydroseq = NULL
)
```

## Arguments

- x:

  A data frame with the identifier column \`id\`, downstream pointer
  \`toid\`, and a numeric \`weight\` column. Terminal/outlet rows use
  \`NA\`, \`""\`, \`"0"\`, or a \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

- weight:

  Character scalar. Column name giving the mainstem weight; at each
  confluence the mainstem follows the largest-weight upstream
  contributor. Typically the arbolate sum (\`accumulate_downstream\` of
  \`lengthkm\`) or total drainage area.

- hydroseq:

  Optional column name of a precomputed hydrosequence to use for level
  path ids. If \`NULL\` (default), it is computed with
  \[get_hydroseq()\].

## Value

Numeric vector of level path ids aligned to the rows of \`x\` (the
hydrosequence of each level path's outlet reach).

## Details

The network must be acyclic (errors otherwise, like
\[accumulate_downstream()\]). Weight ties are broken by first
occurrence; named-river continuity (overriding the weight to hold a
named mainstem together through a confluence) is not modeled.

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# 4 -> 3 -> 1 (mainstem, longer), 2 -> 1 (tributary); 1 -> outlet
df <- data.frame(
  flowpath_id   = c(1, 2, 3, 4),
  flowpath_toid = c(0, 1, 1, 3),
  arb_sum       = c(14, 2, 7, 4)
)
get_levelpath(df, weight = "arb_sum")
#> [1] 4 1 4 4
# reaches 1,3,4 share one level path; reach 2 is its own
```
