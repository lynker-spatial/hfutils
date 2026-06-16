# Package index

## Hydrofabric I/O

Read and write hydrofabric layers from GeoPackages, and lazily query any
GDAL/OGR vector source with dplyr.

- [`read_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/read_hydrofabric.md)
  : Read Hydrofabric Layers from a GeoPackage (or accept in-memory
  \`sf\` objects)
- [`write_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/write_hydrofabric.md)
  : Write a hydrofabric GeoPackage (mixed sf + non-sf)
- [`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
  : Delayed read for vector resources
- [`layer_exists()`](https://lynker-spatial.github.io/hfutils/reference/layer_exists.md)
  : Check if a layer exists in a geopackage
- [`OGRSQL()`](https://lynker-spatial.github.io/hfutils/reference/OGRSQL.md)
  : OGRSQL OGRSQL driver, use to \[dbConnect()\] to a data source
  readable by sf

## Cloud & GeoParquet

Authenticated cloud access and GeoParquet/Arrow round-trips for sf
objects.

- [`lynker_spatial_auth()`](https://lynker-spatial.github.io/hfutils/reference/lynker_spatial_auth.md)
  : Authenticate with Lynker Spatial

- [`lynker_spatial_token()`](https://lynker-spatial.github.io/hfutils/reference/lynker_spatial_token.md)
  : Get an OAuth2 token for Lynker Spatial

- [`lynker_spatial_refresh()`](https://lynker-spatial.github.io/hfutils/reference/lynker_spatial_refresh.md)
  : Refresh an existing Lynker Spatial token

- [`duckdb_connection()`](https://lynker-spatial.github.io/hfutils/reference/duckdb_connection.md)
  : Create a new DuckDB connection

- [`tbl_http()`](https://lynker-spatial.github.io/hfutils/reference/tbl_http.md)
  : Read DuckDB File(s) over HTTP

- [`st_read_parquet()`](https://lynker-spatial.github.io/hfutils/reference/st_read_parquet.md)
  :

  Read a Parquet file to `sf` object

- [`st_write_parquet()`](https://lynker-spatial.github.io/hfutils/reference/st_write_parquet.md)
  :

  Write `sf` object to Parquet file

- [`read_sf_dataset()`](https://lynker-spatial.github.io/hfutils/reference/read_sf_dataset.md)
  : Read Parquet Dataset

- [`write_sf_dataset()`](https://lynker-spatial.github.io/hfutils/reference/write_sf_dataset.md)
  : Write Parquet Dataset

## Geometry cleaning

Repair and simplify DEM-derived catchment geometry, and fast unions by
ID.

- [`clean_geometry()`](https://lynker-spatial.github.io/hfutils/reference/clean_geometry.md)
  : Clean Catchment Geometry
- [`clean_gpkg_layer()`](https://lynker-spatial.github.io/hfutils/reference/clean_gpkg_layer.md)
  : Clean polygon topology in a GeoPackage layer using GDAL and
  Mapshaper
- [`union_polygons()`](https://lynker-spatial.github.io/hfutils/reference/union_polygons.md)
  : Fast polygon union by ID
- [`union_linestrings()`](https://lynker-spatial.github.io/hfutils/reference/union_linestrings.md)
  : Fast linestring union by ID
- [`flowpaths_to_linestrings()`](https://lynker-spatial.github.io/hfutils/reference/flowpaths_to_linestrings.md)
  : Convert MULITLINESTINGS to LINESTRINGS

## Network & topology

Algorithms over the directed (DAG) flow network.

- [`accumulate_downstream()`](https://lynker-spatial.github.io/hfutils/reference/accumulate_downstream.md)
  : Accumulate an attribute downstream over a directed acyclic network
- [`get_hydroseq()`](https://lynker-spatial.github.io/hfutils/reference/get_hydroseq.md)
  : Compute and add the hydrosequence to a directed acyclic network
- [`add_measures()`](https://lynker-spatial.github.io/hfutils/reference/add_measures.md)
  : Add length and area measures to flowpaths/divides
- [`add_areasqkm()`](https://lynker-spatial.github.io/hfutils/reference/add_areasqkm.md)
  : Compute area in square kilometers (numeric)
- [`add_lengthkm()`](https://lynker-spatial.github.io/hfutils/reference/add_lengthkm.md)
  : Compute length in kilometers (numeric)
- [`get_node()`](https://lynker-spatial.github.io/hfutils/reference/get_node.md)
  : Get endpoint or startpoint of LINESTRING
- [`node_geometry()`](https://lynker-spatial.github.io/hfutils/reference/node_geometry.md)
  : Node geometry from line endpoints
- [`rename_geometry()`](https://lynker-spatial.github.io/hfutils/reference/rename_geometry.md)
  : Rename geometry column of sf object

## Versioning & quality assurance

Stamp dataset versions and provenance into GeoPackages, and run staged
pipeline invariant checks shared across the stack.

- [`gpkg_set_version()`](https://lynker-spatial.github.io/hfutils/reference/gpkg_set_version.md)
  : Stamp a dataset version into a GeoPackage's metadata tables
- [`gpkg_get_version()`](https://lynker-spatial.github.io/hfutils/reference/gpkg_get_version.md)
  : Read the dataset version written by \[gpkg_set_version()\]
- [`hf_check_invariants()`](https://lynker-spatial.github.io/hfutils/reference/hf_check_invariants.md)
  : Pipeline invariant checks
- [`hf_check_attr_bounds()`](https://lynker-spatial.github.io/hfutils/reference/hf_check_attr_bounds.md)
  : Check divide/flowpath attribute values against physical-range bounds
- [`hf_attr_bounds()`](https://lynker-spatial.github.io/hfutils/reference/hf_attr_bounds.md)
  : Physical-range bounds for divide and flowpath attributes

## DBI / OGR-SQL backend

Low-level S4 classes and methods backing the lazy
[`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
interface.

- [`st_as_sf(`*`<tbl_OGRSQLConnection>`*`)`](https://lynker-spatial.github.io/hfutils/reference/st_as_sf.md)
  : Force collection of a OGR query Convert as_ogr to a data frame or sf
  object
- [`show(`*`<OGRSQLConnection>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  [`dbSendQuery(`*`<OGRSQLConnection>`*`,`*`<ANY>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  [`dbReadTable(`*`<OGRSQLConnection>`*`,`*`<character>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  [`dbListTables(`*`<OGRSQLConnection>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  [`dbExistsTable(`*`<OGRSQLConnection>`*`,`*`<ANY>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  [`dbDisconnect(`*`<OGRSQLConnection>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLConnection-class.md)
  : Class OGRSQLConnection (and methods)
- [`dbDataType(`*`<OGRSQLDriver>`*`,`*`<ANY>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLDriver-class.md)
  [`dbIsValid(`*`<OGRSQLDriver>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLDriver-class.md)
  [`dbUnloadDriver(`*`<OGRSQLDriver>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLDriver-class.md)
  [`dbGetInfo(`*`<OGRSQLDriver>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLDriver-class.md)
  : Class OGRSQLDriver
- [`show(`*`<OGRSQLResult>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLResult-class.md)
  [`dbFetch(`*`<OGRSQLResult>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLResult-class.md)
  [`dbClearResult(`*`<OGRSQLResult>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLResult-class.md)
  [`dbHasCompleted(`*`<OGRSQLResult>`*`)`](https://lynker-spatial.github.io/hfutils/reference/OGRSQLResult-class.md)
  : Class OGRSQLResult (and methods)
- [`dbConnect(`*`<OGRSQLDriver>`*`)`](https://lynker-spatial.github.io/hfutils/reference/dbConnect-OGRSQLDriver-method.md)
  : dbConnect
