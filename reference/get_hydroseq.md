# Compute and add the hydrosequence to a directed acyclic network.

Compute and add the hydrosequence to a directed acyclic network.

## Usage

``` r
get_hydroseq(x, id = "flowpath_id", toid = "flowpath_toid")
```

## Arguments

- x:

  A data frame (or tibble) containing at least the identifier column
  given by \`id\` and the downstream pointer column given by \`toid\`.

- id:

  Character scalar. Column name in \`topology\` with unique node
  identifiers. Defaults to \`"flowpath_id"\`.

- toid:

  Character scalar. Column name in \`topology\` with the \*downstream\*
  node identifier for each row. Use \`NA\` or \`0\` for
  outlets/terminals. Defaults to \`"flowpath_toid"\`.

- colname:

  Character scalar. Column name to use in result. Defaults to
  \`"hydroseq"\`

## Value

The data frame \`topology\` with an additional column, named
\`colname\`, representing the hydrosequence.
