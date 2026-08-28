# Is a flowpath network a directed acyclic graph?

Is a flowpath network a directed acyclic graph?

## Usage

``` r
hf_network_is_dag(
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

\`TRUE\` if acyclic (or edge-free), \`FALSE\` if any cycle exists.
