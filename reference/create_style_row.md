# Build one \`layer_styles\` row for a GeoPackage layer

Assembles a single row in the schema QGIS expects in a GeoPackage's
\`layer_styles\` table. The geometry column is looked up from
\`gpkg_geometry_columns\`, so a layer without geometry (an attribute
table such as \`network\`) yields \`NULL\` rather than an error: QGIS
styles apply to spatial layers only.

## Usage

``` r
create_style_row(gpkg_path, layer_name, style_name, style_qml)
```

## Arguments

- gpkg_path:

  Path to the GeoPackage.

- layer_name:

  Layer the style applies to.

- style_name:

  Name recorded for the style.

- style_qml:

  QML contents, from \[read_qml()\].

## Value

A one-row data frame, or \`NULL\` if \`layer_name\` has no geometry
column in \`gpkg_path\`.

## Examples

``` r
if (FALSE) { # \dontrun{
row <- create_style_row("hydrofabric.gpkg", "nexus", "nexus_style",
  read_qml(system.file("qml", "nexus.qml", package = "hfutils")))
} # }
```
