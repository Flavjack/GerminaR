# Germination Synchronization Index

This function calculates the germination synchronization of the
germination process.

## Usage

``` r
ger_SYN(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the values of Germination synchrony

## Details

The Synchory Index `Z` has been proposed to assess the degree of overlap
between flowering individuals in a population. By adopting the idea
expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other
included in the same replication. `Z` = 1 when germination of all the
seeds occurs at the same time and `Z` = 0 when at least two seeds can
germinate one each time. `Z` produces a number if and only if there are
two seeds finishing the seed germination process at the same time. Thus,
the value of `Z` assessments is the grade of overlap between seed
germination.

## References

RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination
process? Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar.
2006.

## Examples

``` r
library(GerminaR)
dfr <- prosopis
syn <- ger_SYN(evalName = "D", data = dfr)
syn
#>  [1] 0.63020408 0.66612245 0.55591837 0.75428571 0.92163265 0.88489796
#>  [7] 1.00000000 0.96000000 0.58122449 0.48000000 0.78448980 0.72571429
#> [13] 0.81632653 0.72571429 0.84979592 0.88489796 0.38652482 0.40514184
#> [19] 0.39222942 0.36224490 0.64795918 0.74612245 0.71631206 0.75428571
#> [25] 0.30712303 0.29521277 0.34485816 0.35374150 0.48081633 0.59918367
#> [31] 0.50775510 0.57909343 0.39037928 0.26734505 0.29855072 0.40816327
#> [37] 0.44057971 0.39982270 0.36077706 0.28029602 0.92163265 1.00000000
#> [43] 0.96000000 1.00000000 0.88489796 0.92163265 0.96000000 0.96000000
#> [49] 0.88489796 0.88031915 0.92081633 0.88031915 0.50620567 0.42517007
#> [55] 0.45744681 0.36725254 0.46768707 0.37322695 0.35458937 0.53826531
#> [61] 0.47183673 0.36258865 0.30527211 0.59102041 0.32897959 0.40780142
#> [67] 0.37387755 0.36309524 0.26666667 0.26666667 0.16666667 0.30000000
#> [73] 0.08888889 0.31111111 0.36111111 0.16363636        NaN        NaN
#> [79]        NaN        NaN
```
