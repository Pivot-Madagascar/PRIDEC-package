
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of PRIDEC is to provide a standardized API functionality for
forecasting infectious diseases from DHIS2 data.

## Installation

You can install the development version of PRIDEC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Pivot-Madagascar/PRIDEC-package")
```

## Example

TThe PRIDE-C forecasting approach follows the following steps:

1.  Data processing
2.  Model tuning and training
3.  Forecasting

![](API-workflow.png)

The example below follows the steps for fitting a Random Forest model
using `ranger` from simulated model data.

``` r
library(PRIDEC)

data(demo_malaria)

#preprocess data set
data_clean <- prep_data(raw_data = demo_malaria,
                        y_var = "n_case",
                        lagged_vars =  c("rain_mm", "temp_c"),
                        scaled_vars = NULL,
                        graph_poly = NULL)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#create a cv_fold of assessment (historical) and analysis (forecast) data
cv_set <- split_cv_rolling(data_clean$data_prep, 
                           month_analysis = 48, 
                           month_assess=3)[[20]]

#fit the model and forecast
rf_fit <- fit_ranger(cv_set, 
                     y_var = "n_case",
                     id_vars = c("orgUnit", "date"),
                     pred_vars = c("rain_mm", "temp_c", "month_season", "orgUnit"))

#evaluate model fit
eval_performance(rf_fit)
#> Registered S3 method overwritten by 'scoringutils':
#>   method         from    
#>   print.forecast forecast
#> # A tibble: 2 × 10
#>   dataset    wis   mae med_ae mean_ae_log  wape dispersion sp_rho prop_over
#>   <chr>    <dbl> <dbl>  <dbl>       <dbl> <dbl>      <dbl>  <dbl>     <dbl>
#> 1 analysis  5.87  6.44  0.238      0.0175 0.149       5.10  0.964    0.197 
#> 2 assess   24.2  32.6  31.6        1.08   0.595       2.35  0.797    0.0317
#> # ℹ 1 more variable: prop_under <dbl>
plot_predictions(rf_fit[rf_fit$orgUnit %in% sample(rf_fit$orgUnit,1),])
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
