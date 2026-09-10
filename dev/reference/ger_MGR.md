# Mean Germination Rate

This function calculates the mean germination rate of the germination.

## Usage

``` r
ger_MGR(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the values of Mean Germination Rate

## Details

The average speed of germination is defined as the reciprocal of the
average time germination (RANAL; SANTANA, 2006).

## References

RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination
process? Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar.
2006.

## Examples

``` r
library(GerminaR)
dfr <- prosopis
mgr <- ger_MGR(evalName = "D", data = dfr)
mgr
#>  [1] 0.7812500 0.8196721 0.7575758 0.8771930 0.9615385 0.9433962 1.0000000
#>  [8] 0.9803922 0.5263158 0.5882353 0.5319149 0.5434783 0.9090909 0.8620690
#> [15] 0.9259259 0.9433962 0.3750000 0.3692308 0.3949580 0.3450704 0.5104167
#> [22] 0.5154639 0.5454545 0.5376344 0.1857708 0.1832061 0.1825095 0.1835206
#> [29] 0.3164557 0.3378378 0.3448276 0.3381295 0.1496815 0.1649123 0.1493506
#> [36] 0.1503067 0.2311558 0.2307692 0.2248804 0.2248804 0.9615385 1.0000000
#> [43] 0.9803922 1.0000000 0.9433962 0.9615385 0.9803922 0.9803922 0.8928571
#> [50] 0.9411765 0.9433962 0.9411765 0.4285714 0.4260870 0.4210526 0.4433962
#> [57] 0.6049383 0.5783133 0.4600000 0.5833333 0.3401361 0.3966942 0.3684211
#> [64] 0.3649635 0.2958580 0.2981366 0.2976190 0.2951807 0.3000000 0.3157895
#> [71] 0.3076923 0.2941176 0.1470588 0.1351351 0.1525424 0.1392405       NaN
#> [78]       NaN       NaN       NaN
```
