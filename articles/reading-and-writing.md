# Reading and writing a hydrofabric

``` r

library(hfutils)
```

`hfutils` is the I/O base layer of the hydrofabric stack. It offers
three complementary access patterns:

1.  **[`read_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/read_hydrofabric.md)
    /
    [`write_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/write_hydrofabric.md)**
    — eager, convenience round trips for the common `divides` +
    `flowpaths` realization.
2.  **[`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)**
    — a lazy, dplyr-compatible view over *any* GDAL/OGR vector source
    (local or cloud), materialized only on
    [`collect()`](https://lynker-spatial.github.io/hfutils/reference/st_as_sf.md)
    /
    [`st_as_sf()`](https://lynker-spatial.github.io/hfutils/reference/st_as_sf.md).
3.  **GeoParquet**
    ([`st_read_parquet()`](https://lynker-spatial.github.io/hfutils/reference/st_read_parquet.md)
    /
    [`st_write_parquet()`](https://lynker-spatial.github.io/hfutils/reference/st_write_parquet.md))
    — columnar, cloud-native storage with GeoPandas-compatible metadata.

## A round trip

[`read_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/read_hydrofabric.md)
auto-detects flowpath and divide layers by name, and can harmonize or
reproject coordinate reference systems on read. Here we stage a tiny
two-layer GeoPackage and read it back:

``` r

library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# Stand-in geometries (the North Carolina sample shipped with sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5, ]

tmp <- tempfile(fileext = ".gpkg")
write_hydrofabric(
  list(divides = nc, flowpaths = st_cast(nc, "MULTILINESTRING")),
  outfile = tmp,
  verbose = FALSE
)

hf <- read_hydrofabric(tmp, verbose = FALSE)
names(hf)
#> [1] "flowpaths" "divides"
nrow(hf$divides)
#> [1] 5
```

[`read_hydrofabric()`](https://lynker-spatial.github.io/hfutils/reference/read_hydrofabric.md)
also accepts in-memory `sf` objects directly (skip the GeoPackage), a
`realization` argument to read only `"divides"` or `"flowpaths"`, and a
`crs` argument to transform outputs on the way out.

``` r

fp_only <- read_hydrofabric(tmp, realization = "flowpaths", crs = 5070,
  verbose = FALSE)
st_crs(fp_only$flowpaths)$epsg
#> [1] 5070
```

## Lazy access with `as_ogr()`

[`as_ogr()`](https://lynker-spatial.github.io/hfutils/reference/as_ogr.md)
returns a lazy table you can manipulate with dplyr verbs; nothing is
read until you
[`collect()`](https://lynker-spatial.github.io/hfutils/reference/st_as_sf.md)
(data frame) or
[`st_as_sf()`](https://lynker-spatial.github.io/hfutils/reference/st_as_sf.md)
(spatial). This is the efficient way to pull a single VPU out of a
continental file without loading the whole layer:

``` r

library(dplyr)

as_ogr("conus_nextgen.gpkg", "divides") |>
  filter(vpuid == "01") |>
  select(divide_id, areasqkm) |>
  st_as_sf()
```

Because GDAL backs the connection, the same call works against cloud
objects via GDAL virtual file systems — see
[`vignette("cloud-and-versioning")`](https://lynker-spatial.github.io/hfutils/articles/cloud-and-versioning.md)
for authenticated `/vsis3/` access.

## GeoParquet

For analytical workflows, write to GeoParquet (single file) or a
partitioned dataset:

``` r

st_write_parquet(hf$divides, "divides.parquet")
back <- st_read_parquet("divides.parquet")
```
