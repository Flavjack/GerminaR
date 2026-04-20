# Cumulative sum of germination by period of time for line graphic

This function makes a data table with the cumulative sum of values of
germination by days.

## Usage

``` r
ger_intime(Factor, SeedN, evalName, method = "percentage", data)
```

## Arguments

- Factor:

  Factor which will be graph in time

- SeedN:

  Name of the column with the seed numbers

- evalName:

  Prefix of the evaluation variable

- method:

  Type of cumulative germination. "percentage" or "relative"

- data:

  Data with the germination evaluation process

## Value

Data frame with the germination by period

## Details

Need a summary by factor before use it with function SummaryBy.

## Examples

``` r
if (FALSE) { # \dontrun{

library(GerminaR)
data <- prosopis
grt <- ger_intime(Factor = "temp"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "rel"
                  , data = data)
                   
head(grt, 10)
 
fplot(data = grt
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , groups = "temp"
      , sig = NULL) 
      
} # }
      
```
