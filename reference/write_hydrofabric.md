# Write a hydrofabric GeoPackage (mixed sf + non-sf)

Write a hydrofabric GeoPackage (mixed sf + non-sf)

## Usage

``` r
write_hydrofabric(network_list, outfile, verbose = TRUE, enforce_dm = TRUE)
```

## Arguments

- network_list:

  named list of layers (may include \`sf\` and plain data.frames)

- outfile:

  path to \`.gpkg\` (".gpkg" appended if missing)

- verbose:

  logical, show progress via \`cli\`

- enforce_dm:

  logical, enforce \`hf_dm\` schema (column presence)

## Value

\`outfile\` (invisibly)
