# Nested-set upstream index over a rooted-tree network

Assigns each node two integers that turn "everything upstream of X" into
an O(1) range filter, with no traversal at query time. A depth-first
pre-order walk from each outlet numbers the nodes so that every node and
all of its upstream contributors occupy one contiguous block:

## Usage

``` r
upstream_index(x, id = "flowpath_id", toid = "flowpath_toid")
```

## Arguments

- x:

  A data frame with the identifier column \`id\` and downstream pointer
  \`toid\`. Terminal/outlet rows use \`NA\`, \`""\`, \`"0"\`, or a
  \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

## Value

A data frame aligned to the rows of \`x\` with integer columns
\`upstream_id\` and \`num_upstreams\`, plus attributes \`n_outlets\`,
\`n_divergences\`, and \`n_bad\`.

## Details

- \`upstream_id\`:

  The pre-order position of the node.

- \`num_upstreams\`:

  The count of nodes strictly upstream (its up-tree size, excluding
  itself).

The nodes strictly upstream of a node with \`upstream_id == u\` and
\`num_upstreams == k\` are exactly those whose \`upstream_id\` lies in
the half-open interval \`(u, u + k\]\` (this is the \*nested set
model\*).

Exact only on a rooted tree (single downstream per node). A node with
two downstreams is a divergence the index cannot represent: counted in
\`n_divergences\` and warned. The network must be acyclic (errors
otherwise, like \[accumulate_downstream()\], which supplies
\`num_upstreams\`). Confluences expand the largest-upstream branch first
so main stems stay contiguous; this affects only which block a branch
lands in, never the nested-set property. \`upstream_id\` is
build-specific and changes with topology, so it is not a persistent key.

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
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md)

## Examples

``` r
# two headwaters (1,2) join at 3 -> outlet 4
idx <- upstream_index(data.frame(
  flowpath_id   = c("1", "2", "3", "4"),
  flowpath_toid = c("3", "3", "4", "0")))
# nodes strictly upstream of 4 (upstream_id u, num_upstreams k -> (u, u+k]):
u <- idx$upstream_id[4]; k <- idx$num_upstreams[4]
which(idx$upstream_id > u & idx$upstream_id <= u + k)
#> [1] 1 2 3
```
