# Package index

## All functions

- [`OGRSQL()`](OGRSQL.md) : OGRSQL OGRSQL driver, use to \[dbConnect()\]
  to a data source readable by sf

- [`accumulate_downstream()`](accumulate_downstream.md) : Accumulate an
  attribute downstream over a directed acyclic network

- [`add_areasqkm()`](add_areasqkm.md) : Compute km2 area

- [`add_lengthkm()`](add_lengthkm.md) : Compute length in kilometers
  (numeric)

- [`add_measures()`](add_measures.md) : Add length and area measures to
  flowpaths/divides

- [`as_ogr()`](as_ogr.md) : Delayed read for vector resources

- [`clean_geometry()`](clean_geometry.md) : Clean Catchment Geometry

- [`dbConnect(`*`<OGRSQLDriver>`*`)`](dbConnect-OGRSQLDriver-method.md)
  : dbConnect

- [`duckdb_connection()`](duckdb_connection.md) : Create a new DuckDB
  connection.

- [`flowpaths_to_linestrings()`](flowpaths_to_linestrings.md) : Convert
  MULITLINESTINGS to LINESTRINGS

- [`get_hydroseq()`](get_hydroseq.md) : Compute and add the
  hydrosequence to a directed acyclic network.

- [`get_node()`](get_node.md) : Get endpoint or startpoint of LINESTRING

- [`layer_exists()`](layer_exists.md) : Check if a layer exists in a
  geopackage

- [`lynker_spatial_auth()`](lynker_spatial_auth.md) : Authenticate with
  Lynker Spatial

- [`lynker_spatial_refresh()`](lynker_spatial_refresh.md) : Refresh an
  existing Lynker Spatial token

- [`lynker_spatial_token()`](lynker_spatial_token.md) : Get an OAuth2
  token for Lynker Spatial

- [`node_geometry()`](node_geometry.md) : Node geometry from line
  endpoints

- [`read_hydrofabric()`](read_hydrofabric.md) : Read Hydrofabric Layers
  from a GeoPackage (or accept in-memory \`sf\` objects)

- [`read_sf_dataset()`](read_sf_dataset.md) : Read Parquet Dataset

- [`rename_geometry()`](rename_geometry.md) : Rename geometry column of
  sf object

- [`st_as_sf(`*`<tbl_OGRSQLConnection>`*`)`](st_as_sf.md) : Force
  collection of a OGR query Convert as_ogr to a data frame or sf object

- [`st_read_parquet()`](st_read_parquet.md) :

  Read a Parquet file to `sf` object

- [`st_write_parquet()`](st_write_parquet.md) :

  Write `sf` object to Parquet file

- [`tbl_http()`](tbl_http.md) : Read DuckDB File(s) over HTTP

- [`union_linestrings()`](union_linestrings.md) : Fast linestring union
  by ID

- [`union_polygons()`](union_polygons.md) : Fast polygon union by ID

- [`write_hydrofabric()`](write_hydrofabric.md) : Write a hydrofabric
  GeoPackage (mixed sf + non-sf)

- [`write_sf_dataset()`](write_sf_dataset.md) : Write Parquet Dataset
