# Germinated Seed Number

This function calculates the number of seed germinated.

## Usage

``` r
ger_GRS(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

Number of seed germinated

## Examples

``` r
library(GerminaR)
dt <- prosopis
grs <- ger_GRS(evalName = "D", data = dt)
grs
#>  [1] 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 48 48 47 49 49 50 48 50 47
#> [26] 48 48 49 50 50 50 47 47 47 46 49 46 48 47 47 50 50 50 50 50 50 50 50 50 48
#> [51] 50 48 48 49 48 47 49 48 46 49 50 48 49 50 50 48 50 49  6  6  4  5 10 10  9
#> [76] 11  0  0  0  0
```
