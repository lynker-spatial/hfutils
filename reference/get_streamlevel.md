# Compute stream level over a directed acyclic network

Uses the same topological approach as \[get_levelpath()\] (igraph
topo-sort), operating on the \*level-path\* graph rather than the reach
graph. Stream level counts the number of level-path steps from a reach
to the network terminus: the mainstem level path that drains out of the
network is level \`1\`, every level path that empties into a level-\`1\`
path is level \`2\`, and so on (the NHDPlus \`StreamLeve\` attribute).
All reaches on a level path share its level.

## Usage

``` r
get_streamlevel(
  x,
  id = "flowpath_id",
  toid = "flowpath_toid",
  levelpath = "levelpath"
)
```

## Arguments

- x:

  A data frame with the identifier column \`id\`, downstream pointer
  \`toid\`, and a precomputed \`levelpath\` column (e.g. from
  \[get_levelpath()\]). Terminal/outlet rows use \`NA\`, \`""\`,
  \`"0"\`, or a \`toid\` that is not a known \`id\`.

- id, toid:

  Column names. Default \`"flowpath_id"\` / \`"flowpath_toid"\`.

- levelpath:

  Character scalar. Column name of the level-path id each reach belongs
  to. Default \`"levelpath"\`.

## Value

Integer vector of stream levels aligned to the rows of \`x\` (\`1\` on
the terminal mainstem, increasing up each tributary level path).

## Details

The level-path network must be acyclic (errors otherwise). A level path
is a contiguous mainstem, so it empties into exactly one downstream
level path; the level is a single downstream-first pass over that
coarser graph, mirroring \[get_pathlength()\] / \[get_levelpath()\].

## See also

Other network properties:
[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md),
[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md),
[`get_levelpath()`](https://lynker-spatial.github.io/hfutils/reference/get_levelpath.md),
[`get_pathlength()`](https://lynker-spatial.github.io/hfutils/reference/get_pathlength.md),
[`get_pfafstetter()`](https://lynker-spatial.github.io/hfutils/reference/get_pfafstetter.md),
[`get_streamorder()`](https://lynker-spatial.github.io/hfutils/reference/get_streamorder.md),
[`hf_upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/hf_upstream_index.md),
[`merge_groups()`](https://lynker-spatial.github.io/hfutils/reference/merge_groups.md),
[`upstream_index()`](https://lynker-spatial.github.io/hfutils/reference/upstream_index.md)

## Examples

``` r
# mainstem 4 -> 3 -> 1 (level path A), tributary 2 -> 1 (level path B)
df <- data.frame(
  flowpath_id   = c("1", "2", "3", "4"),
  flowpath_toid = c("0", "1", "1", "3"),
  levelpath     = c("A", "B", "A", "A")
)
get_streamlevel(df)
#> [1] 1 2 1 1
# reaches on A (1,3,4) = 1; tributary 2 (level path B) = 2
```
