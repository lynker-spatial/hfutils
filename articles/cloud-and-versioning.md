# Cloud access, versioning, and quality assurance

``` r

library(hfutils)
```

This vignette covers the three responsibilities that make `hfutils` the
trusted base layer of the stack: **authenticated cloud access**,
**dataset versioning & provenance**, and **staged invariant checks**.

## Authenticated cloud access

[`lynker_spatial_auth()`](https://lynker-spatial.github.io/hfutils/reference/lynker_spatial_auth.md)
provisions (or refreshes) an OAuth2 token and wires it into the
libraries you name. With GDAL configured, any
[`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
/
[`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html)
call can read directly from cloud storage via a GDAL virtual file
system; with DuckDB configured, the bearer token is attached as a
secret.

``` r

# Interactive browser flow; sets GDAL_HTTP_BEARER for the session
token <- lynker_spatial_auth(libs = "gdal")

as_ogr("/vsis3/lynker-spatial/hydrofabric/v2.2/conus/conus_nextgen.gpkg",
       "divides")

# DuckDB over HTTP, with the token attached to the connection
con <- duckdb_connection(extensions = "httpfs")
tbl_http("https://.../flowpaths.parquet", conn = con)
```

## Dataset versioning & provenance

[`gpkg_set_version()`](https://lynker-spatial.github.io/hfutils/reference/gpkg_set_version.md)
stamps a dataset version into the standard GeoPackage metadata tables —
distinct from the GeoPackage *specification* version in
`PRAGMA user_version`, which is left untouched. It records a
machine-readable packed integer (`Mmmpp`), a human-readable semver
string, an optional SPDX license, and an optional JSON provenance
record. It is idempotent: re-stamping replaces prior entries rather than
accumulating duplicates.

``` r

library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

f <- tempfile(fileext = ".gpkg")
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:2, ]
st_write(nc, f, "divides", quiet = TRUE)

gpkg_set_version(
  f, "2.2.0",
  provenance = list(software = "hydrofabric", vpuid = "09"),
  license    = "ODbL-1.0"
)

str(gpkg_get_version(f))
#> List of 4
#>  $ version    : chr "2.2.0"
#>  $ int_version: int 20200
#>  $ license    : chr "ODbL-1.0"
#>  $ provenance :List of 2
#>   ..$ software: chr "hydrofabric"
#>   ..$ vpuid   : chr "09"
```

`int_version` (`20200` here) is the value GDAL surfaces as
`GPKG_METADATA_ITEM_1`, convenient for machine comparison across
releases.

## Staged invariant checks

[`hf_check_invariants()`](https://lynker-spatial.github.io/hfutils/reference/hf_check_invariants.md)
is the shared QA gate used at each stage of the build pipeline. Calling
it *between* steps means a silent failure (a duplicate id, a dangling
`toid`, an orphan divide, a raster ring artifact) halts the run at the
step that created it — not 200 lines later during visual inspection.

It supports four stages — `"refactored"`, `"reconciled"`,
`"aggregated"`, and `"ngen"` — each expecting stage-specific inputs.
Here is the topology half of a `refactored`-stage check on a clean toy
network:

``` r

refactored <- data.frame(
  flowpath_id   = c("101", "102", "103"),
  flowpath_toid = c("102", "103", "0")
)

res <- hf_check_invariants("refactored", refactored = refactored)
#> [invariants:refactored] ✓ refactored_unique                refactored flowpath_id has 0 duplicate(s)
#> [invariants:refactored] ✓ refactored_topology              0 refactored flowpath_toid values reference nonexistent IDs
res$ok
#> [1] TRUE
```

By default any failed check throws (`strict = TRUE`); set
`strict = FALSE` to downgrade failures to warnings while still surfacing
them. A dangling downstream pointer is caught:

``` r

broken <- data.frame(
  flowpath_id   = c("101", "102"),
  flowpath_toid = c("102", "999")   # 999 does not exist
)

hf_check_invariants("refactored", refactored = broken)
#> [invariants:refactored] ✓ refactored_unique                refactored flowpath_id has 0 duplicate(s)
#> Error:
#> ! [invariants:refactored] ✗ refactored_topology              1 refactored flowpath_toid values reference nonexistent IDs
```

The `aggregated` and `ngen` stages additionally accept `coverage = TRUE`
to run the (more expensive) check that each flowpath lies within its
assigned catchment.
