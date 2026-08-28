# Run arbitrary SQL against a GeoPackage in a single transaction

Each \`...\` item is either a SQL string (executed in order), a
\`list(table=, df=)\` (written as a temporary table first), or a
\`list(drop_triggers_for=)\` (drops that layer's triggers, restored at
commit). All statements run in one transaction; any error rolls back.

## Usage

``` r
gpkg_exec(gpkg, ...)
```

## Arguments

- gpkg:

  Path to the GeoPackage.

- ...:

  SQL strings and/or the list forms described above.

## Value

Invisibly \`NULL\`.

## Updating a spatial layer

Any \`UPDATE\` against a layer that has a geometry column needs a
\`list(drop_triggers_for = \<layer\>)\` ahead of it, including when the
statement touches only attribute columns. GDAL's R-tree triggers call
SpatiaLite functions such as \`ST_IsEmpty()\` that RSQLite does not
provide, so the write fails with \`no such function: ST_IsEmpty\` and
the transaction rolls back. A temporary table staged with \`list(table=,
df=)\` must be referenced with a quoted identifier (\`".\_patch"\`) if
its name starts with a character SQLite would otherwise parse, such as a
dot.
