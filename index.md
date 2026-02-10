# Hydrofabric Utilities

`hfutils` provides the base utilities needed for hydrofabric creation
and manipulation.

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
```

## Basic Use

### I/O

#### Basic connection: Dataset

``` r
gpkg <- '/Users/mikejohnson/hydrofabric/v3.0/reference_fabric.gpkg'
hfutils::as_ogr(gpkg)
#> [1] "divides"        "flowpaths"      "hydrolocations" "pois"          
#> [5] "events"         "network"
#> <OGRSQLConnection>
#>  DSN: /Users/mikejohnson/hydrofabric/v3.0/reference_fabric.gpkg...
#> tables: divides, flowpaths, hydrolocations, pois, events, network
```

#### Basic connection: Layer

``` r
hfutils::as_ogr(gpkg, "divides") 
#> # Source:   table<"divides"> [?? x 6]
#> # Database: OGRSQLConnection
#>    divide_id vpuid areasqkm has_flowpath flowpath_id                        geom
#>        <dbl> <chr>    <dbl> <lgl>              <dbl>          <MULTIPOLYGON [m]>
#>  1 931090014 01     8.61    TRUE           931090014 (((2068815 2313015, 206890…
#>  2 931080009 01     0.158   TRUE           931080009 (((1933035 2277735, 193315…
#>  3 931080008 01     0.296   TRUE           931080008 (((1933035 2277735, 193300…
#>  4 931080007 01     1.99    TRUE           931080007 (((1932855 2277345, 193285…
#>  5 931080006 01     2.33    TRUE           931080006 (((1931025 2278695, 193102…
#>  6 931070173 01     1.21    TRUE           931070173 (((1967025 2413755, 196702…
#>  7 931070172 01     7.72    TRUE           931070172 (((1966455 2412855, 196651…
#>  8 931060011 01     1.78    TRUE           931060011 (((2021925 2487855, 202204…
#>  9 931060010 01     1.85    TRUE           931060010 (((2021925 2487855, 202174…
#> 10 931060009 01     0.00765 TRUE           931060009 (((1990665 2513595, 199057…
#> # ℹ more rows
```

#### Lazy Eval

``` r
hfutils::as_ogr(gpkg, "divides") |> 
  select(divide_id, areasqkm) 
#> # Source:   SQL [?? x 2]
#> # Database: OGRSQLConnection
#>    divide_id areasqkm
#>        <dbl>    <dbl>
#>  1 931090014  8.61   
#>  2 931080009  0.158  
#>  3 931080008  0.296  
#>  4 931080007  1.99   
#>  5 931080006  2.33   
#>  6 931070173  1.21   
#>  7 931070172  7.72   
#>  8 931060011  1.78   
#>  9 931060010  1.85   
#> 10 931060009  0.00765
#> # ℹ more rows
```

#### sf extraction

``` r
hfutils::as_ogr(gpkg, "divides")  |> 
  filter(vpuid == "01") |> 
  st_as_sf()
#> Simple feature collection with 67880 features and 5 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1824825 ymin: 2215935 xmax: 2255715 ymax: 3086925
#> Projected CRS: NAD83 / Conus Albers
#> # A tibble: 67,880 × 6
#>    divide_id vpuid areasqkm has_flowpath flowpath_id                        geom
#>        <dbl> <chr>    <dbl> <lgl>              <dbl>          <MULTIPOLYGON [m]>
#>  1 931090014 01     8.61    TRUE           931090014 (((2068815 2313015, 206890…
#>  2 931080009 01     0.158   TRUE           931080009 (((1933035 2277735, 193315…
#>  3 931080008 01     0.296   TRUE           931080008 (((1933035 2277735, 193300…
#>  4 931080007 01     1.99    TRUE           931080007 (((1932855 2277345, 193285…
#>  5 931080006 01     2.33    TRUE           931080006 (((1931025 2278695, 193102…
#>  6 931070173 01     1.21    TRUE           931070173 (((1967025 2413755, 196702…
#>  7 931070172 01     7.72    TRUE           931070172 (((1966455 2412855, 196651…
#>  8 931060011 01     1.78    TRUE           931060011 (((2021925 2487855, 202204…
#>  9 931060010 01     1.85    TRUE           931060010 (((2021925 2487855, 202174…
#> 10 931060009 01     0.00765 TRUE           931060009 (((1990665 2513595, 199057…
#> # ℹ 67,870 more rows
```

#### Remote Access

``` r
# hfutils::as_ogr('/vsis3/lynker-spatial/hydrofabric/v2.2/conus/conus_nextgen.gpkg')
```

### Network Properties

``` r
## Accumulate Downstream
system.time({
  da <-  hfutils::as_ogr(gpkg, 'flowpaths')  |> 
  filter(vpuid == "01") |> 
  st_as_sf() |> 
  accumulate_downstream(attr = "areasqkm")})
#>    user  system elapsed 
#>   0.911   0.491   1.550

head(da)
#> [1]  15.66720  11.12805   4.44600   6.52365   4.53420 335.51145

## Hydrosequence
system.time({
  hs <-  hfutils::as_ogr(gpkg, 
                         'flowpaths')  |> 
  filter(vpuid == "01") |> 
  st_as_sf() |> 
  get_hydroseq()
})
#>    user  system elapsed 
#>   0.900   0.488   1.398

head(hs)
#> [1]   806 22724 22726 22725 22727 28139
```

### Questions?

Please reach out via an issue or PR if you have comments, concerns, or
questions!
