# Recompute hydrosequence for a flowpath network by topological sort

NHD convention: lower \`hydroseq\` = more downstream (closer to outlet).
Call after any topology-modifying operation (cycle break, toid
reassignment) to keep \`hydroseq\` consistent with the current toid
graph.

## Usage

``` r
hf_recompute_hydroseq(
  flowpaths,
  id_col = "flowpath_id",
  toid_col = "flowpath_toid"
)
```

## Arguments

- flowpaths:

  A data.frame/sf with id and downstream-id columns.

- id_col, toid_col:

  Column names for the node id and its downstream id.

## Value

\`flowpaths\` with an updated integer \`hydroseq\` column.
