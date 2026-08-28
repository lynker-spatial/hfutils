# Hydrofabric Utilities

`hfutils` provides the base utilities needed for hydrofabric I/O +
navigation; geometry modification + cleaning; and authentication to the
Lynker Spatial cloud. Collectively, it provides the building blocks from
which reference fabrics and geoprocessed fabrics can be created.

## Installation

You can install the development version of `hfutils` from
[GitHub](https://github.com/) with:

``` r

# install.packages("remotes")
remotes::install_github("lynker-spatial/hfutils")
```

``` r

library(hfutils)
library(dplyr)
library(sf)
```

## Basic Use

### I/O

#### Basic connection: Dataset

``` r

# point this at a hydrofabric GeoPackage (with flowpaths + divides layers)
gpkg <- "reference_fabric.gpkg"

# With a single user layer, that layer is selected automatically. A source
# holding several layers is ambiguous, so as_ogr() lists them and asks you
# to name one.
hfutils::as_ogr(gpkg)
#> Error: Multiple layers found; please specify `layer` explicitly:
#> > divides, flowpaths
```

#### Basic connection: Layer

``` r

hfutils::as_ogr(gpkg, "divides")
```

#### Lazy Eval

``` r

hfutils::as_ogr(gpkg, "divides") |>
  select(divide_id, areasqkm)
```

#### sf extraction

``` r

hfutils::as_ogr(gpkg, "divides")  |>
  filter(vpuid == "01") |>
  st_as_sf()
```

### Network Properties

``` r

## Accumulate Downstream
system.time({
  da <-  hfutils::as_ogr(gpkg, "flowpaths")  |>
    filter(vpuid == "01") |>
    st_as_sf() |>
    accumulate_downstream(attr = "areasqkm")
})

head(da)

## Hydrosequence
system.time({
  hs <-  hfutils::as_ogr(gpkg, "flowpaths")  |>
    filter(vpuid == "01") |>
    st_as_sf() |>
    get_hydroseq()
})

head(hs)
```

### Questions?

Please reach out via an issue or PR if you have comments, concerns, or
questions!
