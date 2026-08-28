# In-place geometry update for specific rows of a GeoPackage layer

Writes the changed features to a temporary layer in the same GeoPackage,
swaps the geometry column into \`layer\` via a keyed (indexed) SQL
UPDATE, then drops the temp layer. Layer triggers are dropped/restored
around the write (see \[gpkg_update_col()\]).

## Usage

``` r
gpkg_update_geom(gpkg, layer, id_col, sf_changed)
```

## Arguments

- gpkg:

  Path to the GeoPackage.

- layer:

  Target layer name.

- id_col:

  Id column used to match changed rows.

- sf_changed:

  An \`sf\` of the changed features (must carry \`id_col\`).

## Value

Invisibly \`NULL\`.

## Details

Removing the temporary layer means removing everything GDAL created with
it: the feature table, its four \`rtree\_\*\` index shadow tables and
their triggers, and its rows in \`gpkg_contents\`,
\`gpkg_geometry_columns\`, \`gpkg_extensions\`, and
\`gpkg_ogr_contents\`. All of it happens inside the same transaction as
the geometry swap, so a failure leaves the GeoPackage exactly as it was
found.
