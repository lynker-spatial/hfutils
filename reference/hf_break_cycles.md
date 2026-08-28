# Break cycles in a flowpath network by severing one back-edge per SCC

For each strongly-connected component with \>1 member, the node
receiving the most in-edges from OUTSIDE the SCC is its outlet; every
other member whose toid points to that outlet (the back-edge completing
the cycle) has its toid set to \`"0"\`, preserving as much downstream
connectivity as possible while making the graph a DAG.

## Usage

``` r
hf_break_cycles(flowpaths, id_col = "flowpath_id", toid_col = "flowpath_toid")
```

## Arguments

- flowpaths:

  A data.frame/sf with id and downstream-id columns.

- id_col, toid_col:

  Column names for the node id and its downstream id.

## Value

\`flowpaths\` with \`toid_col\` rewritten to remove cycles.
