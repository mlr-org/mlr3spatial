# Leipzig Land Cover Task

Point survey of land cover in Leipzig. Includes Sentinel-2 spectral
bands and NDVI.

## Source

Copernicus Sentinel Data (2021). Retrieved from Copernicus Open Access
Hub and processed by European Space Agency.

## Examples

``` r
if (requireNamespace("sf")) {
  library(sf)
  data("leipzig", package = "mlr3spatial")
  print(leipzig)
}
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
#> Simple feature collection with 97 features and 9 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 731930.5 ymin: 5692136 xmax: 733220.3 ymax: 5693968
#> Projected CRS: WGS 84 / UTM zone 32N
#> # A tibble: 97 × 10
#>      b02   b03   b04   b06   b07   b08   b11    ndvi land_cover
#>  * <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl> <chr>     
#>  1   903   772   426  2998  4240  4029  1816  0.809  forest    
#>  2  1270  1256  1081  1998  2493  2957  2073  0.465  urban     
#>  3  1033   996   777  2117  2748  2799  1595  0.565  urban     
#>  4   962   773   500   465   505   396   153 -0.116  water     
#>  5  1576  1527  1626  1715  1745  1768  1980  0.0418 urban     
#>  6  1125  1185   920  3058  3818  3758  2682  0.607  pasture   
#>  7   880   746   424  2502  3500  3397  1469  0.778  forest    
#>  8  1332  1251  1385  1663  1799  1640  1910  0.0843 urban     
#>  9   940   741   475   452   515   400   139 -0.0857 water     
#> 10   902   802   454  2764  3821  3666  1567  0.780  forest    
#> # ℹ 87 more rows
#> # ℹ 1 more variable: geom <POINT [m]>
```
