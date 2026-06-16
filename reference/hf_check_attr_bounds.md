# Check divide/flowpath attribute values against physical-range bounds

Complements the topology/geometry checks in \[hf_check_invariants()\]
with a per-attribute physical-plausibility pass: every modeling
parameter present in the layer is checked against its expected range.
Attributes not present in the layer are skipped; bounds carrying a
\`caveat\` are skipped unless \`trust_caveated = TRUE\`. Failures are
reported but never abort, even when \`strict = TRUE\`.

## Usage

``` r
hf_check_attr_bounds(
  layer,
  which = c("divides", "flowpaths"),
  domain = NULL,
  strict = FALSE,
  trust_caveated = FALSE,
  max_report = 5L
)
```

## Arguments

- layer:

  A data.frame or sf object (the Divides or Flowpaths layer).

- which:

  One of \`"divides"\` or \`"flowpaths"\`.

- domain:

  Domain code for \`lat\`/\`lon\` bounds: one of \`"CONUS"\`, \`"AK"\`,
  \`"HI"\`, \`"PRVI"\`, or \`NULL\` to skip lat/lon.

- strict:

  Logical. Passed through to the reporter; attribute-bound failures are
  soft and warn regardless.

- trust_caveated:

  Logical. Include bounds flagged in the \`caveat\` column (default
  \`FALSE\`).

- max_report:

  Integer. Max example offending values to show per attribute.

## Value

Invisibly a list with \`ok\` and \`checks\` (same shape as
\[hf_check_invariants()\]).
