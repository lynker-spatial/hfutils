# Group a network into contiguous same-order runs for partitioning

Walks the nodes in nested-set pre-order (\[upstream_index()\]) and
breaks a run where the current node does not flow directly into the
previous one, or (with \`order\`) where stream order changes. Each group
is a contiguous \`upstream_id\` range, so a size-budgeted tiler can
merge groups into complete sub-networks. Adapted from
[joshcu/upstream-index](https://github.com/joshcu/upstream-index).

## Usage

``` r
merge_groups(
  x,
  id = "flowpath_id",
  toid = "flowpath_toid",
  order = NULL,
  upstream_id = NULL
)
```

## Arguments

- x:

  A data frame with the identifier column \`id\` and downstream pointer
  \`toid\`. Terminal/outlet rows use \`NA\`, \`""\`, \`"0"\`, or a
  \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

- order:

  Optional column name of a stream order (e.g. Strahler); when given, a
  run also breaks where the order changes. \`NULL\` breaks only on
  connectivity (pure mainstem runs).

- upstream_id:

  Optional column name of a precomputed \`upstream_id\`
  (\[upstream_index()\]); computed internally when \`NULL\`.

## Value

Integer vector of group ids aligned to the rows of \`x\`.

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# a mainstem 4 -> 3 -> 1 with a tributary 2 -> 1, all order 1
x <- data.frame(
  flowpath_id   = c("1", "2", "3", "4"),
  flowpath_toid = c("0", "1", "1", "3"),
  ord           = c(1, 1, 1, 1))
merge_groups(x, order = "ord")
#> [1] 1 2 1 1
```
