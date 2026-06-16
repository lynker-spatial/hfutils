# Clean polygon topology in a GeoPackage layer using GDAL and Mapshaper

Extracts a polygon layer from a GeoPackage, reprojects it to EPSG:4326,
runs \`mapshaper -clean\` to repair polygon topology, reprojects the
cleaned output back to the original coordinate reference system, and
overwrites the original layer in the source GeoPackage.

## Usage

``` r
clean_gpkg_layer(
  gpkg,
  layer,
  mapshaper = "mapshaper",
  ogr2ogr = "ogr2ogr",
  gdalsrsinfo = "gdalsrsinfo",
  snap_interval = NULL,
  gap_fill_area = NULL,
  overlap_rule = NULL,
  verbose = FALSE
)
```

## Arguments

- gpkg:

  Character scalar. Path to the input GeoPackage.

- layer:

  Character scalar. Name of the layer in \`gpkg\` to clean.

- mapshaper:

  Character scalar. Path to the \`mapshaper\` executable. Defaults to
  \`"mapshaper"\`, assuming it is available on the system path.

- ogr2ogr:

  Character scalar. Path to the \`ogr2ogr\` executable. Defaults to
  \`"ogr2ogr"\`.

- gdalsrsinfo:

  Character scalar. Path to the \`gdalsrsinfo\` executable. Defaults to
  \`"gdalsrsinfo"\`.

- snap_interval:

  Optional numeric or character scalar. Passed to \`mapshaper -clean\`
  as \`snap-interval=...\`. Use this to snap nearby vertices before
  topology repair. Because cleaning occurs in EPSG:4326, this value is
  interpreted in decimal degrees.

- gap_fill_area:

  Optional numeric or character scalar. Passed to \`mapshaper -clean\`
  as \`gap-fill-area=...\`. Controls the maximum enclosed gap area that
  will be filled.

- overlap_rule:

  Optional character scalar. Passed to \`mapshaper -clean\` as
  \`overlap-rule=...\`. Controls how overlap areas are assigned. Common
  values include \`"max-area"\`, \`"min-area"\`, \`"max-id"\`, and
  \`"min-id"\`.

- verbose:

  Logical scalar. If \`TRUE\`, print step-by-step progress, commands,
  and captured CRS detection output.

## Value

Invisibly returns a named list with:

- gpkg:

  Path to the GeoPackage that was modified.

- layer:

  Layer name that was cleaned.

- original_srs:

  The CRS definition used to project the cleaned layer back to its
  original coordinate system.

- tmp_files:

  Named character vector of temporary intermediate files used during
  processing. These are scheduled for deletion on exit.

## Details

This function is intended for polygon datasets that may contain small
overlaps, enclosed gaps, or minor boundary misalignments that prevent a
clean polygon mosaic. The topology repair is performed in geographic
coordinates because \`mapshaper -clean\` is often more reliable there
for mixed-source inputs, after which the data are restored to their
original CRS.

The function relies on external command line tools:

- \`ogr2ogr\` for layer extraction, reprojection, and overwrite

- \`gdalsrsinfo\` for recovering the original CRS definition

- \`mapshaper\` for polygon cleaning

CRS detection prefers an EPSG code, then a PROJ string, and finally a
simple WKT representation as a fallback.

The processing steps are:

1.  Extract the requested layer from the source GeoPackage into a
    temporary single-layer GeoPackage.

2.  Detect the original CRS using \`gdalsrsinfo\`, preferring EPSG
    output.

3.  Reproject the temporary layer to EPSG:4326 and write it as GeoJSON.

4.  Run \`mapshaper -clean\` on the EPSG:4326 GeoJSON.

5.  Reproject the cleaned output back to the original CRS.

6.  Overwrite the original layer in the input GeoPackage.

Temporary files are created in \`tempdir()\` and removed with
\`on.exit()\`.

The function assumes the target layer contains polygonal data
appropriate for \`mapshaper -clean\`. It may still run on other geometry
types, but the repair behavior is designed for polygon topology.

## Requirements

The following executables must be installed and available:

- GDAL tools: \`ogr2ogr\`, \`gdalsrsinfo\`

- Mapshaper CLI

On many systems, Mapshaper can be installed with:


    npm install -g mapshaper

## Caveats

- Cleaning is performed in EPSG:4326, so \`snap_interval\` is
  interpreted in angular units, not the source layer's native units.

- Overwriting replaces the original layer in the GeoPackage.

- Attribute preservation depends on the behavior of \`ogr2ogr\` and
  \`mapshaper\`; field ordering and some metadata may change.

- Very large layers may require substantial temporary disk space.

## Examples

``` r
if (FALSE) { # \dontrun{
clean_gpkg_layer(
  gpkg = "data/hydrofabric.gpkg",
  layer = "divides"
)

clean_gpkg_layer(
  gpkg = "data/hydrofabric.gpkg",
  layer = "catchments",
  snap_interval = 1e-7,
  overlap_rule = "max-area",
  verbose = TRUE
)

clean_gpkg_layer(
  gpkg = "data/hydrofabric.gpkg",
  layer = "basins",
  gap_fill_area = 0,
  mapshaper = "/usr/local/bin/mapshaper",
  ogr2ogr = "/opt/homebrew/bin/ogr2ogr",
  gdalsrsinfo = "/opt/homebrew/bin/gdalsrsinfo"
)
} # }
```
