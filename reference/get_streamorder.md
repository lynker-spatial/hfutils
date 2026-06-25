# Compute and add Strahler stream order to a directed acyclic network

Native replacement for \`hydroloom::add_streamorder\` – same topological
approach as \[get_hydroseq()\] (igraph topo-sort), with no external
dependency and no non-dendritic/divergence handling required. Leaves are
order 1; at each node the order is the max of its upstream contributors,
incremented by 1 when that max is shared by two or more of them
(Strahler).

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

## Examples

``` r
# two headwaters (1,2) join at 3 -> outlet: 3 is order 2
get_streamorder(data.frame(flowpath_id = c("1", "2", "3"),
  flowpath_toid = c("3", "3", "0")))
#> [1] 1 1 2
```
