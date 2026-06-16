# Network properties: accumulation and hydrosequence

``` r

library(hfutils)
```

A hydrofabric flow network is a **directed acyclic graph (DAG)**: every
feature (`id`) points to exactly one downstream feature (`toid`), and
terminal features point to `0` (or `NA`). `hfutils` provides two
topology primitives that operate on this structure — both are
character-safe, so identifiers such as `"fp-123"` or scientific-notation
strings round-trip cleanly.

## Downstream accumulation

[`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md)
propagates a per-feature attribute (drainage area, incremental length,
anything additive) downstream, summing all upstream contributions at
confluences. It runs a single topological sort followed by one O(E) edge
pass, so it scales to continental networks.

Consider two headwaters (`1`, `2`) joining at `3`, which flows to the
outlet `4`:

``` r

df <- data.frame(
  flowpath_id   = c("1", "2", "3", "4"),
  flowpath_toid = c("3", "3", "4", "0"),
  area          = c(1.0, 2.0, 0.5, 0.0)
)

accumulate_downstream(df, attr = "area")
#> [1] 1.0 2.0 3.5 3.5
```

Feature `3` accumulates `1.0 + 2.0 + 0.5 = 3.5`, and the outlet inherits
the full basin total. The column names default to `flowpath_id` /
`flowpath_toid` but can be overridden via the `id` and `toid` arguments
to match any network table (including reference-fabric `id`/`toid`).

A cyclic network is rejected rather than silently producing wrong
totals:

``` r

cyclic <- data.frame(
  flowpath_id   = c("1", "2"),
  flowpath_toid = c("2", "1"),
  area          = c(1, 1)
)

accumulate_downstream(cyclic, attr = "area")
#> Error in `accumulate_downstream()`:
#> ! Network contains cycles; cannot accumulate.
```

## Hydrosequence

[`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md)
assigns a total topological ordering to the network. Larger values are
upstream; the value decreases monotonically as you move toward the
outlet, which makes it a convenient sort key for downstream traversal.

``` r

df$hydroseq <- get_hydroseq(df)
df[order(-df$hydroseq), c("flowpath_id", "flowpath_toid", "hydroseq")]
#>   flowpath_id flowpath_toid hydroseq
#> 4           4             0        4
#> 2           2             3        3
#> 1           1             3        2
#> 3           3             4        1
```

## In practice

Both functions accept a data frame, tibble, or `sf` object. A typical
pattern reads a layer lazily, filters to a VPU, materializes it, and
accumulates:

``` r

library(dplyr)

da <- as_ogr("conus_nextgen.gpkg", "flowpaths") |>
  filter(vpuid == "01") |>
  st_as_sf() |>
  accumulate_downstream(attr = "areasqkm")
```

See
[`vignette("reading-and-writing")`](https://lynker-spatial.github.io/hfutils/articles/reading-and-writing.md)
for the I/O side of that pipeline.
