# Germination Uncertainty

This function calculates the germination uncertainty in the germination
process.

## Usage

``` r
ger_UNC(evalName, data)
```

## Arguments

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the values of Germination Uncertainty.

## Details

The uncertainty index `u` is an adaptation of Shannon index measures the
degree of uncertainty in predicting the informational entropy or
uncertainty associated with the distribution of the relative frequency
of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). Low
values of `u` indicate frequencies with short peaks, i.e. the more
concentrated the germination in time. Just a germinated seed changes the
value of `u`. This means that `u` measures the degree of germination
scattering.

## References

GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington.
LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA,
Washington, DC, 1983.

## Examples

``` r
library(GerminaR)
dfr <- prosopis
unc <- ger_UNC(evalName = "D", data = dfr)
unc
#>  [1] 0.9461447 0.8157272 0.9043815 0.5842388 0.2422922 0.3274449 0.0000000
#>  [8] 0.1414405 1.0844751 1.1985488 0.5293609 0.6343096 0.4689956 0.6343096
#> [15] 0.4021792 0.3274449 1.4171327 1.4081359 1.5822405 1.4950825 0.9281698
#> [22] 0.7050757 0.6500224 0.5842388 1.6951591 1.8213883 1.6026878 1.5043742
#> [29] 1.5468954 1.0302088 1.2633065 1.0697797 1.6179042 1.9947498 1.9243519
#> [36] 1.5071571 1.2646502 1.3775500 1.6034362 1.9685404 0.2422922 0.0000000
#> [43] 0.1414405 0.0000000 0.3274449 0.2422922 0.1414405 0.1414405 0.3274449
#> [50] 0.3372901 0.2822922 0.3372901 1.1228074 1.3718323 1.2987949 1.4883676
#> [57] 1.1796780 1.4531143 1.5098718 1.0214779 1.4015264 1.6336284 1.8160410
#> [64] 1.1510457 1.7518407 1.5487081 1.5566689 1.6343886 1.4591479 1.4591479
#> [71] 1.5000000 1.3709506 2.6464393 1.4854753 1.3516441 2.1626441 0.0000000
#> [78] 0.0000000 0.0000000 0.0000000
```
