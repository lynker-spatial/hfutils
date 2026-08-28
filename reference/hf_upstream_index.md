# Nested-set upstream index for a hydrofabric (flowpath + nexus grain)

Resolves the \`flowpath -\> nexus -\> flowpath\` topology to a direct
flowpath graph and runs \[upstream_index()\] over it. Shared by
\[write_hydrofabric()\] so a hydrofabric is indexed the same way however
it is written. A divergent nexus (two downstreams) is flagged; the index
is exact only on a rooted tree.

## Usage

``` r
hf_upstream_index(
  flowpaths,
  nexus = NULL,
  fp_id = "flowpath_id",
  fp_toid = "flowpath_toid",
  nex_id = "nexus_id",
  nex_toid = "nexus_toid"
)
```

## Arguments

- flowpaths:

  Data frame with \`flowpath_id\` and \`flowpath_toid\`.

- nexus:

  Data frame with \`nexus_id\` and \`nexus_toid\`, or \`NULL\` (a
  \`flowpath_toid\` that is not a flowpath is then a terminal).

- fp_id, fp_toid:

  Flowpath id / downstream column names.

- nex_id, nex_toid:

  Nexus id / downstream column names.

## Value

A data frame with \`flowpath_id\`, \`upstream_id\`, \`num_upstreams\`
aligned to \`flowpaths\` (attributes \`n_outlets\`, \`n_divergences\`,
\`n_bad\`).

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
fp  <- data.frame(flowpath_id = c("1","2","3"), flowpath_toid = c("nex-9","0","nex-9"))
nex <- data.frame(nexus_id = "nex-9", nexus_toid = "2")
hf_upstream_index(fp, nex)
#>   flowpath_id upstream_id num_upstreams
#> 1           1           3             0
#> 2           2           1             2
#> 3           3           2             0
```
