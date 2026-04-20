# Multiple comparison test

Function analysis of variance for summary data.

## Usage

``` r
ger_testcomp(aov, comp, type = "snk", sig = 0.05)
```

## Arguments

- aov:

  lm o aov result function.

- comp:

  treatments will be compared.

- type:

  method for made comparison analysis: c("snk", "tukey", "duncan").

- sig:

  significance level. Default 0.05

## Value

Table with complete data for graphics

## Examples

``` r
 

if (FALSE) { # \dontrun{

library(GerminaR)
library(dplyr)

gdata <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))

smr <- ger_summary(SeedN = "seeds", evalName = "D", data = gdata)

av <- aov(grp ~ rep + nacl*temp, smr)

mc <- ger_testcomp(aov = av
                   , comp = c("nacl", "temp")
                   )
                   
} # } 
```
