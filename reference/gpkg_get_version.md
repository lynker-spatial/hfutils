# Read the dataset version written by \[gpkg_set_version()\]

Reads back the version entries from a GeoPackage's metadata tables.
Returns \`NULL\` if the GeoPackage has no metadata table or no version
entries – so it is safe to call on any GeoPackage.

## Usage

``` r
gpkg_get_version(gpkg)
```

## Arguments

- gpkg:

  Path to a GeoPackage file.

## Value

A named \`list\` with \`version\` (semver string), \`int_version\`
(integer), \`license\` (SPDX id, or \`NULL\`), and \`provenance\`
(parsed JSON, or \`NULL\`); or \`NULL\` if no version metadata is
present.
