
<!-- README.md is generated from README.Rmd. Please edit that file -->
I Know How You Drive (ikhyd)
============================

<!-- badges: start -->
<!-- badges: end -->
The goal of ikhyd is to make the analysis process of telematics data easier.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("issactoast/ikhyd")
```

Example
-------

### Calibation of telematics data

Using `calibrate_telematics` function, you can obtain a calibarated longitudinal, latitudinal acceleration data and GPS data from telematics data.

``` r
library(ikhyd)

## basic example code
sample_data_path <- system.file("extdata", "sample_trip.csv",
                                package = "ikhyd")

calibration_result <- calibrate_telematics(sample_data_path)

acc_data <- calibration_result$acc_data
gps_data <- calibration_result$gps_data
```

-   Result (acc\_data)
    -   x: Lateral acceleration
    -   y: Longditudinal acceleration

``` r
head(acc_data)
#>    time          x           y
#> 1 0.000 0.00000000  0.00000000
#> 2 0.037 0.03259792  0.04009324
#> 3 0.076 0.03850935 -0.03077333
#> 4 0.114 0.03678785 -0.03858204
#> 5 0.154 0.03550664 -0.03372845
#> 6 0.194 0.04023494 -0.05466185
```

-   Result (gps\_data)
    -   x: Longditude
    -   y: Latitude

``` r
head(gps_data)
#>    time        x        y
#> 1 0.000 -91.6134 41.69637
#> 2 0.037 -91.6134 41.69637
#> 3 0.076 -91.6134 41.69637
#> 4 0.114 -91.6134 41.69637
#> 5 0.154 -91.6134 41.69637
#> 6 0.194 -91.6134 41.69637
```
