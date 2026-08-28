# Format numeric identifiers without scientific notation

\`as.character()\` on a round-number double id (e.g. \`22000000\`)
yields \`"2.2e+07"\`, which then fails every downstream integer-string
join and silently drops the record from the network (lineage loss). This
formats numeric ids in plain-digit form, preserves a \`".part"\` split
suffix verbatim (\`".10"\` must not round-trip through \`as.numeric()\`
to \`".1"\`), re-normalizes an already-scientific string back to plain
digits, and keeps \`NA\` as \`NA\`.

## Usage

``` r
hf_fmt_id(v)
```

## Arguments

- v:

  A vector of identifiers, numeric or character.

## Value

A character vector of plain-digit ids, with \`NA\` preserved.

## Examples

``` r
hf_fmt_id(22000000)      # "22000000", not "2.2e+07"
#> [1] "22000000"
hf_fmt_id("123.10")      # "123.10" (split suffix preserved)
#> [1] "123.10"
```
