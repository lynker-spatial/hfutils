# Clean Catchment Geometry

Fix common geometry issues in DEM-derived catchments (disjoint
fragments, artifacts, invalid topologies) and optionally apply topology-
preserving simplification via mapshaper. If \`flowlines\` are provided,
the largest part per catchment is chosen preferentially where it touches
the corresponding flowline feature.

## Usage

``` r
clean_geometry(
  catchments,
  flowlines = NULL,
  fl_ID = NULL,
  ID = "ID",
  keep = NULL,
  crs = 5070,
  grid = 9e-04,
  gb = 8,
  force = FALSE,
  sys = NULL
)
```

## Arguments

- catchments:

  sf POLYGON/MULTIPOLYGON with a unique ID column.

- flowlines:

  optional sf LINESTRING/MULTILINESTRING for picking the "prime" part
  per catchment (largest piece that intersects the flowline).

- fl_ID:

  character. Unique identifier column in \`flowlines\` that matches
  \`ID\` in \`catchments\`.

- ID:

  character. Unique identifier column in \`catchments\`. Default: "ID".

- keep:

  numeric in (0,1\]. Proportion of points to retain in simplification.
  If \`NULL\`, no simplification is performed. Default: \`NULL\`.

- crs:

  integer or object accepted by \`sf::st_crs\`. Should be a projected
  CRS suitable for area calculations. Default: 5070 (NAD83 / Conus
  Albers).

- grid:

  numeric. Snap-to-grid size (in target CRS units). Default: 0.0009.

- gb:

  integer. Heap GB for \`mapshaper-xl\` when \`force = TRUE\` and \`gb
  \> 8\`. Default: 8.

- force:

  logical. Use system \`mapshaper\` / \`mapshaper-xl\` binaries directly
  for simplification (faster and more robust on large data). Default:
  FALSE.

- sys:

  logical or NULL. Use system mapshaper for rmapshaper calls. If NULL,
  auto-detect. Set env \`TURN_OFF_SYS_MAPSHAPER=YUP\` to force
  \`FALSE\`.

## Value

sf with the same ID column and fixed geometry; includes \`areasqkm\`.
