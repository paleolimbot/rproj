
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rproj

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paleolimbot/rproj/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/rproj/actions)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/rproj/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/rproj?branch=master)
<!-- badges: end -->

The goal of rproj is to provide a literal wrapper around the PROJ C
API. It uses [libproj](https://github.com/paleolimbot/libproj) as a
standalone copy of PROJ independent of a system installation.

## Installation

You can install the development version from [R
Universe](https://r-universe.dev/) with:

``` r
install.packages("rproj", repos = "https://paleolimbot.r-universe.dev")
```

## Example

Create a transform and transform some coordinates:

``` r
library(rproj)
pipe <- proj_create_crs_to_crs("OGC:CRS84", "EPSG:3827")
proj_trans(pipe, proj_coord(-64, 45))
#>             x        y  z  t errno
#> [1,] 486517.9 15013159 NA NA     0
```

You can transform arbitrary R objects using the [crs2crs
interface](https://github.com/paleolimbot/crs2crs):

``` r
library(crs2crs)
crs_set_engine(crs_engine_rproj())

library(sf)
#> Linking to GEOS 3.8.1, GDAL 3.2.1, PROJ 7.2.1
nc <- read_sf(system.file("shape/nc.shp", package = "sf"))

nc %>% 
  crs_set_longlat(datum = "NAD27") %>% 
  crs_transform("EPSG:3857")
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9386879 ymin: 4012991 xmax: -8399788 ymax: 4382079
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # … with 90 more rows, and 4 more variables: BIR79 <dbl>, SID79 <dbl>,
#> #   NWBIR79 <dbl>, geometry <MULTIPOLYGON [m]>
```
