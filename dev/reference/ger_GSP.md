# Germination Speed

This function calculates the Germination Speed according at the time
lapse of the evaluations.

## Usage

``` r
ger_GSP(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the Germination Speed

## Examples

``` r
library(GerminaR)
dfr <- prosopis
gsp <- ger_GSP(evalName = "D", data = dfr)
gsp
#>  [1]  78.12500  81.96721  75.75758  87.71930  96.15385  94.33962 100.00000
#>  [8]  98.03922  52.63158  58.82353  53.19149  54.34783  90.90909  86.20690
#> [15]  92.59259  94.33962  37.50000  36.92308  39.49580  34.50704  51.04167
#> [22]  51.54639  54.54545  53.76344  18.57708  18.32061  18.25095  18.35206
#> [29]  31.64557  33.78378  34.48276  33.81295  14.96815  16.49123  14.93506
#> [36]  15.03067  23.11558  23.07692  22.48804  22.48804  96.15385 100.00000
#> [43]  98.03922 100.00000  94.33962  96.15385  98.03922  98.03922  89.28571
#> [50]  94.11765  94.33962  94.11765  42.85714  42.60870  42.10526  44.33962
#> [57]  60.49383  57.83133  46.00000  58.33333  34.01361  39.66942  36.84211
#> [64]  36.49635  29.58580  29.81366  29.76190  29.51807  30.00000  31.57895
#> [71]  30.76923  29.41176  14.70588  13.51351  15.25424  13.92405       NaN
#> [78]       NaN       NaN       NaN
```
