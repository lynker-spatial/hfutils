# Compute and add the hydrosequence to a directed acyclic network

Compute and add the hydrosequence to a directed acyclic network

## Usage

``` r
get_hydroseq(x, id = "flowpath_id", toid = "flowpath_toid")
```

## Arguments

- x:

  A data frame (or tibble) containing at least the identifier column
  given by \`id\` and the downstream pointer column given by \`toid\`.

- id:

  Character scalar. Column name in \`x\` with unique node identifiers.
  Defaults to \`"flowpath_id"\`.

- toid:

  Character scalar. Column name in \`x\` with the \*downstream\* node
  identifier for each row. Use \`NA\` or \`0\` for outlets/terminals.
  Defaults to \`"flowpath_toid"\`.

## Value

A numeric vector of hydrosequence values aligned to the rows of \`x\`
(largest values upstream, decreasing downstream).

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamlevel()`](https://lynker-spatial.github.io/hfutils/reference/get_streamlevel.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# 1 -> 2 -> 3 (outlet). Headwater "1" gets the largest hydroseq.
df <- data.frame(
  flowpath_id   = c("1", "2", "3"),
  flowpath_toid = c("2", "3", "0")
)
get_hydroseq(df)
#> [1] 2 1 3
```
