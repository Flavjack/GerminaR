---
title: "GerminaR 1.6"
author: "Flavio Lozano-Isla, Omar Benites-Alfaro, Denise Garcia de Santana, Marli A. Ranal, Marcelo Francisco Pompelli"
date: "2020-08-10"
output: 
      rmarkdown::html_vignette:
        toc: true
        toc_depth: 4
        keep_md: true
vignette: >
  %\VignetteIndexEntry{GerminaR 1.6}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

The package `GerminaR` has been developed to calculate different germination indices and graphical functions to analyze punctual and accumulative germination. For calculating the indices is necessary acumulative germination data. For more details, you can read the description of each index, the seed germination dataset and analysis in the germinar's user manual. ([GerminaQuant](https://flavjack.github.io/germinaquant/))

First we load the `GerminaR` package. It provides the `prosopis` dataset set that we will work throughout all the examples. 

## Data: GerminaR

The `prosopis` dataset contains information from an experiment containing information from germination experiment with *Prosopis juliflor* under different osmotic potentials and temperatures evaluated during 10 days.


```r
library(GerminaR)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(GerminaR)
dt <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))
```

## List of the principal functions

* `ger_summary`
* `ger_intime`
* `fplot` 

## Interactive shiny application

The function`GerminaQuant()` activates an interactive application with friendly interface for performing the different germination, statistical and graphic analysis. For activation of some function could be necessary internet connection


```r
GerminaQuant()
```

## Summary of Germination Variables

The function `ger_summary()`, according to the accumulative germination data, calculates several germination indices maintaining the values of each experimental unit and experiments factor for statistical analysis.


```r
smr <- ger_summary(SeedN = "seeds"
                   , evalName = "D"
                   , data = dt
                   )

knitr::kable(head(smr, 10),align = "c")
```



| rep | nacl | temp | seeds | grs | grp | mgt  |    mgr    |    gsp    |    unc    |    syn    |    vgt    |    sdg    |   cvg    |
|:---:|:----:|:----:|:-----:|:---:|:---:|:----:|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|:--------:|
|  1  |  0   |  25  |  50   | 50  | 100 | 1.28 | 0.7812500 | 78.12500  | 0.9461447 | 0.6302041 | 0.3281633 | 0.5728554 | 44.75433 |
|  2  |  0   |  25  |  50   | 50  | 100 | 1.22 | 0.8196721 | 81.96721  | 0.8157272 | 0.6661224 | 0.2159184 | 0.4646702 | 38.08772 |
|  3  |  0   |  25  |  50   | 50  | 100 | 1.32 | 0.7575758 | 75.75758  | 0.9043815 | 0.5559184 | 0.2220408 | 0.4712121 | 35.69788 |
|  4  |  0   |  25  |  50   | 50  | 100 | 1.14 | 0.8771930 | 87.71930  | 0.5842388 | 0.7542857 | 0.1228571 | 0.3505098 | 30.74648 |
|  1  |  0   |  30  |  50   | 50  | 100 | 1.04 | 0.9615385 | 96.15385  | 0.2422922 | 0.9216327 | 0.0391837 | 0.1979487 | 19.03353 |
|  2  |  0   |  30  |  50   | 50  | 100 | 1.06 | 0.9433962 | 94.33962  | 0.3274449 | 0.8848980 | 0.0575510 | 0.2398979 | 22.63188 |
|  3  |  0   |  30  |  50   | 50  | 100 | 1.00 | 1.0000000 | 100.00000 | 0.0000000 | 1.0000000 | 0.0000000 | 0.0000000 | 0.00000  |
|  4  |  0   |  30  |  50   | 50  | 100 | 1.02 | 0.9803922 | 98.03922  | 0.1414405 | 0.9600000 | 0.0200000 | 0.1414214 | 13.86484 |
|  1  | 0.5  |  25  |  50   | 50  | 100 | 1.90 | 0.5263158 | 52.63158  | 1.0844751 | 0.5812245 | 0.3775510 | 0.6144518 | 32.33957 |
|  2  | 0.5  |  25  |  50   | 50  | 100 | 1.70 | 0.5882353 | 58.82353  | 1.1985488 | 0.4800000 | 0.3775510 | 0.6144518 | 36.14422 |

On the other hand, you can analyze each variable independently using the following germination indexes.

## Plot function

`fplot()` is generic plot function optimized for publication graphs and you can add modification using `ggplot2` package.


```r
grt <- ger_intime(Factor = "nacl"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = dt)

fplot(data = grt
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , groups = "nacl"
      , ylab = "Germination ('%')"
      , xlab = "days"
      , glab  = "NaCl (mM)"
      , legend = "top"
      , sig = NULL
      , color = T
      )
```

![](D:/omar/Github/GerminaR/vignettes/GerminaR_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

