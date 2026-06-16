# Write a hydrofabric GeoPackage (mixed sf + non-sf)

Write a hydrofabric GeoPackage (mixed sf + non-sf)

## Usage

``` r
write_hydrofabric(network_list, outfile, verbose = TRUE, enforce_dm = FALSE)
```

## Arguments

- network_list:

  named list of layers (may include \`sf\` and plain data.frames)

- outfile:

  path to \`.gpkg\` (".gpkg" appended if missing)

- verbose:

  logical, show progress via \`cli\`

- enforce_dm:

  logical, enforce a data-model schema (column presence) by validating
  each layer against an \`hf_dm\` object found in scope. Defaults to
  \`FALSE\`; \`hf_dm\` is not shipped with hfutils, so enable this only
  when a caller (e.g. the \`hydrofabric\` build package) provides
  \`hf_dm\`.

## Value

\`outfile\` (invisibly)
