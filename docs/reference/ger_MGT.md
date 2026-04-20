# Mean Germination Time

This function calculates the mean germination time of germination
according at the time lapse of the evaluations.

## Usage

``` r
ger_MGT(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the values of Mean Germination Time.

## Details

It was proposed by Haberlandt in 1875. It is calculated as the weighted
average germination time. The number of germinated seeds at the
intervals established for the collection of data is used as weight. It
is expressed in terms of the same units of time used in the germination
count (CZABATOR, 1962).

## References

CZABATOR, F. J. Germination value: an index combining speed and
completeness of pine seed germination. Forest Science, v. 8, n. 4, p.
386-396, 1962.

## Examples

``` r
library(GerminaR)
dfr <- prosopis
mgt <- ger_MGT(evalName = "D", data = dfr)
mgt
#>  [1] 1.280000 1.220000 1.320000 1.140000 1.040000 1.060000 1.000000 1.020000
#>  [9] 1.900000 1.700000 1.880000 1.840000 1.100000 1.160000 1.080000 1.060000
#> [17] 2.666667 2.708333 2.531915 2.897959 1.959184 1.940000 1.833333 1.860000
#> [25] 5.382979 5.458333 5.479167 5.448980 3.160000 2.960000 2.900000 2.957447
#> [33] 6.680851 6.063830 6.695652 6.653061 4.326087 4.333333 4.446809 4.446809
#> [41] 1.040000 1.000000 1.020000 1.000000 1.060000 1.040000 1.020000 1.020000
#> [49] 1.120000 1.062500 1.060000 1.062500 2.333333 2.346939 2.375000 2.255319
#> [57] 1.653061 1.729167 2.173913 1.714286 2.940000 2.520833 2.714286 2.740000
#> [65] 3.380000 3.354167 3.360000 3.387755 3.333333 3.166667 3.250000 3.400000
#> [73] 6.800000 7.400000 6.555556 7.181818      NaN      NaN      NaN      NaN
```
