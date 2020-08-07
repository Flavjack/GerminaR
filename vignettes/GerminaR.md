---
title: "GerminaR 1.6"
author: "Flavio Lozano-Isla, Omar Benites-Alfaro, Denise Garcia de Santana, Marli A. Ranal, Marcelo Francisco Pompelli"
date: "2020-08-07"
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



| rep | nacl | temp | seeds | D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | grs | grp | mgt  |    mgr    |    gsp    |    unc    |    syn    |    vgt    |    sdg    |   cvg    |
|:---:|:----:|:----:|:-----:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:---:|:---:|:---:|:----:|:---------:|:---------:|:---------:|:---------:|:---------:|:---------:|:--------:|
|  1  |  0   |  25  |  50   | 0  | 39 | 8  | 3  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.28 | 0.7812500 | 78.12500  | 0.9461447 | 0.6302041 | 0.9983673 | 0.9991833 | 78.06120 |
|  2  |  0   |  25  |  50   | 0  | 40 | 9  | 1  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.22 | 0.8196721 | 81.96721  | 0.8157272 | 0.6661224 | 0.7791837 | 0.8827138 | 72.35359 |
|  3  |  0   |  25  |  50   | 0  | 34 | 16 | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.32 | 0.7575758 | 75.75758  | 0.9043815 | 0.5559184 | 0.9926531 | 0.9963198 | 75.47877 |
|  4  |  0   |  25  |  50   | 0  | 43 | 7  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.14 | 0.8771930 | 87.71930  | 0.5842388 | 0.7542857 | 0.5114286 | 0.7151423 | 62.73178 |
|  1  |  0   |  30  |  50   | 0  | 48 | 2  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.04 | 0.9615385 | 96.15385  | 0.2422922 | 0.9216327 | 0.1583673 | 0.3979540 | 38.26480 |
|  2  |  0   |  30  |  50   | 0  | 47 | 3  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.06 | 0.9433962 | 94.33962  | 0.3274449 | 0.8848980 | 0.2338776 | 0.4836089 | 45.62348 |
|  3  |  0   |  30  |  50   | 0  | 50 | 0  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.00 | 1.0000000 | 100.00000 | 0.0000000 | 1.0000000 | 0.0000000 | 0.0000000 | 0.00000  |
|  4  |  0   |  30  |  50   | 0  | 49 | 1  | 0  | 0  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.02 | 0.9803922 | 98.03922  | 0.1414405 | 0.9600000 | 0.0804082 | 0.2835633 | 27.80033 |
|  1  | 0.5  |  25  |  50   | 0  | 10 | 37 | 1  | 2  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.90 | 0.5263158 | 52.63158  | 1.0844751 | 0.5812245 | 1.5612245 | 1.2494897 | 65.76262 |
|  2  | 0.5  |  25  |  50   | 0  | 18 | 30 | 1  | 1  | 0  | 0  | 0  | 0  | 0  |  0  | 50  | 100 | 1.70 | 0.5882353 | 58.82353  | 1.1985488 | 0.4800000 | 1.5448980 | 1.2429392 | 73.11407 |

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
      , z = "nacl"
      , ylab = "Germination ('%')"
      , xlab = "days"
      , lgl = "NaCl (mM)"
      , lgd = "top"
      , color = F
      )
```

