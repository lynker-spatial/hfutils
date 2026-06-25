# Physical-range bounds for divide and flowpath attributes

Returns the attribute plausibility-bounds table shipped with the package
(\`inst/extdata/divide_attr_bounds.csv\`). Each row gives the physically
reasonable range for a divide or flowpath modeling parameter. The
\`caveat\` column flags bounds that are demonstrably too strict or look
like placeholders; those are excluded from checks by default. The
\`aliases\` column (\`;\`-separated) lists alternate column names a
bound also matches, so a single bound covers schema naming differences
(e.g. \`lengthkm\` matches \`length_km\`).

## Usage

``` r
hf_attr_bounds()
```

## Value

A data.frame of bounds (one row per attribute, plus per-domain rows for
\`lat\`/\`lon\`).

## Examples

``` r
if (FALSE) { # \dontrun{
bounds <- hf_attr_bounds()
head(bounds)
} # }
```
