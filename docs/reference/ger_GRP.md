# Germination Seed Percentage

This function calculates the germination percentage related at total
seed sown for experimental unit.

## Usage

``` r
ger_GRP(SeedN, evalName, data)
```

## Arguments

- SeedN:

  Name of the column with the number of seeds sown.

- evalName:

  Prefix of the names of the periods of evaluation.

- data:

  The name of the data frame containing the data.

## Value

It returns an vector with the percentage of seed germinated.

## Details

According GOUVEA LABOURIAU (1983), the germinability of a sample of is
the percentage of seeds in which the seed germination process comes to
an end, in experimental conditions by the seminal intrauterine growth
resulting protrusion (or emergence) of a living embryo.

## References

LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA,
Washington, DC, 1983.

## Examples

``` r
library(GerminaR)
dt <- prosopis
grp <- ger_GRP(SeedN = "seeds",evalName = "D", data = dt)
grp
#>  [1] 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100  96  96  94
#> [20]  98  98 100  96 100  94  96  96  98 100 100 100  94  94  94  92  98  92  96
#> [39]  94  94 100 100 100 100 100 100 100 100 100  96 100  96  96  98  96  94  98
#> [58]  96  92  98 100  96  98 100 100  96 100  98  12  12   8  10  20  20  18  22
#> [77]   0   0   0   0
```
