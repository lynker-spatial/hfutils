# Global-merge (Stage 4) conservation + completeness invariants

Complements the structural merge checks (id-uniqueness, prefixes, toid
resolution, divide FK, DAG) with the conservation/completeness class
that catches SILENT data loss or corruption when per-VPU fabrics are
merged into a global domain: a dropped/duplicated feature, lost
catchment area, a missing VPU, zeroed drainage area (the
\`add_measures\` zero-fill class), mixed CRS, or an untagged feature.
Reusable across any domain merge.

## Usage

``` r
hf_check_merge_invariants(
  merged,
  expected = NULL,
  area_tol = 0.005,
  stage = "merge",
  strict = FALSE
)
```

## Arguments

- merged:

  named list with \`flowpaths\`, \`divides\`, \`nexus\` (sf/data.frame)
  for the merged domain product.

- expected:

  optional list of input expectations summed over the per-VPU inputs:
  \`vpus\` (character), \`n_flowpaths\`, \`n_divides\`, \`area_sqkm\`.
  When a field is absent the corresponding check reports as info
  (skipped).

- area_tol:

  fractional tolerance for divide-area conservation (default 0.005).

- stage:

  message label (default "merge").

- strict:

  if TRUE, a failed check stops execution (else warns).

## Value

invisibly \`list(ok, stage, checks)\`.

## Examples

``` r
if (FALSE) { # \dontrun{
merged <- list(
  flowpaths = sf::read_sf("conus.gpkg", "flowpaths"),
  divides   = sf::read_sf("conus.gpkg", "divides"),
  nexus     = sf::read_sf("conus.gpkg", "nexus")
)
hf_check_merge_invariants(merged,
  expected = list(vpus = sprintf("%02d", 1:18)))
} # }
```
