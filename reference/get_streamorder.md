# Compute and add Strahler stream order to a directed acyclic network

Same topological approach as \[get_hydroseq()\] (igraph topo-sort), with
no non-dendritic/divergence handling required. Leaves are order 1; at
each node the order is the max of its upstream contributors, incremented
by 1 when that max is shared by two or more of them (Strahler).

## Usage

``` r
get_streamorder(x, id = "flowpath_id", toid = "flowpath_toid")
```

## Arguments

- x:

  A data frame with the identifier column \`id\` and downstream pointer
  \`toid\`. Terminal/outlet rows use \`NA\`, \`""\`, \`"0"\`, or a
  \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

## Value

Integer vector of stream orders aligned to the rows of \`x\`.

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# two headwaters (1,2) join at 3 -> outlet: 3 is order 2
get_streamorder(data.frame(flowpath_id = c("1", "2", "3"),
  flowpath_toid = c("3", "3", "0")))
#> [1] 1 1 2
```
