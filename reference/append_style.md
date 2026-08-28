# Write QGIS layer styles into a GeoPackage

Stamps the packaged QML symbology into a GeoPackage's \`layer_styles\`
table, so the file opens pre-styled in QGIS. One row is written per
requested layer that has both a shipped QML and a geometry column; any
existing \`layer_styles\` table is replaced.

## Usage

``` r
append_style(
  gpkg_path,
  qml_dir = system.file("qml", package = "hfutils"),
  layer_names
)
```

## Arguments

- gpkg_path:

  Path to the GeoPackage to stamp.

- qml_dir:

  Directory of \`.qml\` files. Defaults to the set shipped with hfutils,
  covering every geometry-bearing layer in the hydrofabric data model:
  \`divides\` and \`WB\` (polygon), \`flowpaths\` and \`flowlines\`
  (line), and \`hydrolocations\`, \`lakes\` and \`nexus\` (point).

- layer_names:

  Layers to style.

## Value

\`gpkg_path\`, invisibly.

## Details

QML files are matched to layers by exact basename, so \`flowpaths\`
takes \`flowpaths.qml\` and never \`flowlines.qml\`. Requested layers
with no shipped QML, and attribute tables with no geometry column, are
skipped rather than mismatched or errored on.

\`layer_styles\` is a QGIS extension rather than part of the GeoPackage
specification, so it appears as an extra table in the file. \[as_ogr()\]
excludes it from layer discovery by default.

## Examples

``` r
if (FALSE) { # \dontrun{
append_style("hydrofabric.gpkg", layer_names = c("divides", "flowpaths"))
} # }
```
