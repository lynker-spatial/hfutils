# Pipeline invariant checks

Runs a set of invariant assertions at a named pipeline stage. Designed
to be called between pipeline steps so silent failures (orphan divides,
unsplit parents, raster artifacts) halt the run at the step that created
the problem – not 200 lines later during visual inspection.

## Usage

``` r
hf_check_invariants(
  stage,
  ...,
  strict = TRUE,
  coverage = FALSE,
  coverage_min = 0.9,
  attr_bounds = FALSE,
  domain = NULL,
  attr_trust_caveated = FALSE
)
```

## Arguments

- stage:

  Character. One of \`"refactored"\`, \`"reconciled"\`,
  \`"aggregated"\`, \`"ngen"\`.

- ...:

  Stage-specific named arguments (see details).

- strict:

  Logical. If \`TRUE\` (default) any failed check throws an error. If
  \`FALSE\`, failures become warnings.

- coverage:

  Logical. If \`TRUE\`, run the expensive flowpath-in-catchment coverage
  check (aggregated and ngen stages only). Default \`FALSE\`.

- coverage_min:

  Minimum fraction of a flowpath that must lie inside its assigned
  catchment. Default \`0.90\`.

- attr_bounds:

  Logical. If \`TRUE\`, append a per-attribute physical-range
  plausibility pass over the \`divides\` and \`flowpaths\` layers (see
  \[hf_check_attr_bounds()\]). Soft/warn-only and off by default, so it
  never changes the returned \`ok\` for existing callers until enabled.
  Absent attribute columns are skipped.

- domain:

  Domain code (\`"CONUS"\`, \`"AK"\`, \`"HI"\`, \`"PRVI"\`) selecting
  the \`lat\`/\`lon\` bounds for the attribute pass; \`NULL\` skips
  lat/lon.

- attr_trust_caveated:

  Logical. Include attribute bounds flagged in the \`caveat\` column of
  the bounds table (default \`FALSE\`).

## Value

Invisibly a list with \`ok\` (logical), \`stage\`, and \`checks\` (named
list of per-check results).

## Details

Expected arguments per stage:

\- \`stage = "refactored"\`: \`refactored\` (sf, split NHD flowlines),
\`reconciled\` (sf, collapsed-and-reconciled lines with integer
\`ID\`). - \`stage = "reconciled"\`: \`reconciled\` (sf), \`divides\`
(sf of reconciled divides). - \`stage = "aggregated"\`: \`flowpaths\`
(sf), \`divides\` (sf), \`network\` (data.frame, optional). - \`stage =
"ngen"\`: \`flowpaths\` (sf with \`fp-\` IDs), \`divides\` (sf with
\`cat-\` IDs).
