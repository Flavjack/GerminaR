# GerminaQuant data analysis

Function analysis of variance for summary data.

## Usage

``` r
gquant_analysis(
  data,
  response,
  factors,
  block = NA,
  comparison = NA,
  type = "snk",
  sig = 0.05
)
```

## Arguments

- data:

  data from ger_summary() function

- response:

  germination indices to analyse

- factors:

  factor as vector or factor model as string

- block:

  block factor for RCBD

- comparison:

  treatments will be compared.

- type:

  method for made comparison analysis: c("snk", "tukey", "duncan").

- sig:

  significance level. Default 0.05

## Value

list

## Details

Function for the analysis module in the app

## Examples

``` r
 

if (FALSE) { # \dontrun{

library(GerminaR)

smr <- ger_summary(SeedN = "seeds", evalName = "D", data = prosopis)

mc <- gquant_analysis(data = smr
                      , response = "grp"
                      , factors = c("nacl", "temp")
                      , block = "rep"
                      , comparison = c("nacl", "temp")
                      )
                      
mc

} # } 
```
