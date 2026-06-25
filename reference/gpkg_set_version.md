# Stamp a dataset version into a GeoPackage's metadata tables

Records the dataset version in the standard GeoPackage metadata
extension tables (\`gpkg_metadata\` / \`gpkg_metadata_reference\`),
creating and registering the extension if the GeoPackage does not
already have it. Three entries are written at \`geopackage\` scope:

## Usage

``` r
gpkg_set_version(
  gpkg,
  version,
  int_version = semver_to_int(version),
  provenance = NULL,
  license = NULL,
  scope = "geopackage"
)
```

## Arguments

- gpkg:

  Path to a GeoPackage file.

- version:

  Human-readable semantic version, e.g. \`"2.1.4"\`.

- int_version:

  Machine-readable integer version. Defaults to the packed \`Mmmpp\`
  form of \`version\`.

- provenance:

  Optional named \`list\` written as a JSON build-provenance entry (for
  example \`list(software = "hydrofabric 0.1", git_sha = "...",
  build_date = "...")\`). \`NULL\` (default) writes no provenance entry.

- license:

  Optional SPDX license identifier (e.g. \`"CC0-1.0"\`, \`"ODbL-1.0"\`)
  recorded as a metadata entry. \`NULL\` (default) writes none.

- scope:

  GeoPackage metadata reference scope. Default \`"geopackage"\`
  (whole-dataset).

## Value

The \`gpkg\` path, invisibly.

## Details

- a machine-readable integer version (\`Mmmpp\`, \`text/plain\`),
  surfaced by GDAL as \`GPKG_METADATA_ITEM_1\`;

- a human-readable semantic version (\`text/plain\`);

- if \`provenance\` is supplied, a JSON build-provenance record
  (\`application/json\`).

The dataset version is independent of the GeoPackage \*specification\*
version held in \`PRAGMA user_version\`, which is left untouched. The
function is idempotent: re-stamping replaces any prior entries it wrote
rather than accumulating duplicates.

## Examples

``` r
if (FALSE) { # \dontrun{
gpkg_set_version("hydrofabric.gpkg", version = "2.2.0",
  license = "ODbL-1.0",
  provenance = list(software = "hydrofabric 0.1",
    build_date = Sys.Date()))
} # }
```
