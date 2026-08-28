# In-place update of a single non-geometry column in a GeoPackage layer

Updates \`col\` for the rows whose \`id_col\` matches \`id_vals\`, via a
temporary join table. Layer triggers are dropped for the write and
restored afterward (they call SpatiaLite functions unavailable under
RSQLite, which would abort the UPDATE). \`id_vals\` and \`col_vals\` are
parallel vectors of equal length.

## Usage

``` r
gpkg_update_col(gpkg, layer, id_col, id_vals, col, col_vals)
```

## Arguments

- gpkg:

  Path to the GeoPackage.

- layer:

  Target layer (table) name.

- id_col:

  Name of the id column to match on.

- id_vals:

  Ids identifying the rows to update.

- col:

  Name of the (non-geometry) column to update.

- col_vals:

  New values for \`col\`, parallel to \`id_vals\`.

## Value

Invisibly \`NULL\`.
