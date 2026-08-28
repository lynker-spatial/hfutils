# Compute downstream path length to the network outlet over a DAG

Uses the same topological approach as \[get_levelpath()\] /
\[get_hydroseq()\] (igraph topo-sort). Path length is the distance along
the network from the \*downstream end\* (outlet) of each reach to the
terminal outlet of the network: the sum of the lengths of every reach
strictly downstream. It does \*\*not\*\* include the reach's own length,
so terminal (outlet) reaches are \`0\` and the value increases upstream,
matching the NHDPlus \`PathLength\` attribute.

## Usage

``` r
get_pathlength(
  x,
  id = "flowpath_id",
  toid = "flowpath_toid",
  length = "lengthkm"
)
```

## Arguments

- x:

  A data frame with the identifier column \`id\`, downstream pointer
  \`toid\`, and a numeric \`length\` column. Terminal/outlet rows use
  \`NA\`, \`""\`, \`"0"\`, or a \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

- length:

  Character scalar. Column name giving each reach's own length (e.g.
  \`"lengthkm"\`); the returned path length is in the same units.

## Value

Numeric vector of path lengths aligned to the rows of \`x\` (\`0\` at
terminal reaches, increasing upstream).

## Details

The network must be acyclic (errors otherwise, like
\[accumulate_downstream()\]). The downstream path from any reach is
unique in a dendritic network; the single downstream-first pass
finalizes each downstream reach's path length before it is read, so the
whole traversal is O(E) after one topological sort.

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# 1 -> 2 -> 3 (outlet); each reach 5 km long
df <- data.frame(
  flowpath_id   = c("1", "2", "3"),
  flowpath_toid = c("2", "3", "0"),
  lengthkm      = c(5, 5, 5)
)
get_pathlength(df, length = "lengthkm")
#> [1] 10  5  0
# reach 3 (outlet) = 0; reach 2 = 5 (length of 3); reach 1 = 10 (len 2 + len 3)
```
