# Assert a flowpath network is a DAG, reporting any cycle nodes

Unlike a strongly-connected-component test, \`igraph::is_dag\` also
catches self-loop cycles (\`a -\> a\`). \`cycle_ids\` reports every node
in a cycle (multi-node SCCs plus self-loops) for diagnostics/repair.

## Usage

``` r
hf_assert_network_dag(
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

\`list(is_dag, message, cycle_ids)\`.
