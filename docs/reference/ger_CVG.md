# Coefficient of Variance of the Mean Germination Time

This function calculates the coefficient of variation of the mean
germination time

## Usage

``` r
ger_CVG(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the values of Coefficient of Variance of
germination

## Examples

``` r
library(GerminaR)
dfr <- prosopis
cvg <- ger_CVG(evalName = "D", data = dfr)
cvg
#>  [1] 44.75433 38.08772 35.69788 30.74648 19.03353 22.63188  0.00000 13.86484
#>  [9] 32.33957 36.14422 17.46068 20.12652 27.54961 31.92483 25.37477 22.63188
#> [17] 26.04211 32.27728 36.69263 24.64789 23.20194 19.23058 20.54301 18.84461
#> [25] 24.36151 26.44352 26.35138 27.28776 26.63819 16.66282 21.18799 17.20875
#> [33] 11.72228 16.98720 14.38376 14.88942 13.83021 15.30256 16.78174 22.87988
#> [41] 19.03353  0.00000 13.86484  0.00000 22.63188 19.03353 13.86484 13.86484
#> [49] 42.83892 23.02334 29.58827 23.02334 39.86517 36.43710 36.46423 37.51886
#> [57] 33.93191 40.87479 40.34077 29.16667 23.21199 31.68230 33.63209 25.33911
#> [65] 25.28165 22.58815 21.47845 25.43264 24.49490 31.04817 29.45930 26.30668
#> [73] 32.36779 24.83612 29.64891 23.14760      NaN      NaN      NaN      NaN
```
