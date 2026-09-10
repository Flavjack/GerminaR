# Plot line or bar graphic

Function use the dtsm function for plot the results

## Usage

``` r
fplot(
  data,
  type = "bar",
  x,
  y,
  group = NA,
  xlab = NA,
  ylab = NA,
  glab = NA,
  ylimits = NA,
  xrotation = NA,
  xtext = NA,
  gtext = NA,
  legend = "top",
  sig = NA,
  sigsize = 3,
  error = NA,
  color = TRUE,
  opt = NA
)
```

## Arguments

- data:

  Output from ger_testcomp function

- type:

  Type of graphic. "bar" or "line"

- x:

  Axis x variable

- y:

  Axis y variable

- group:

  Group variable

- xlab:

  Title for the axis x

- ylab:

  Title for the axis y

- glab:

  Title for the legend

- ylimits:

  limits of the y axis

- xrotation:

  Rotation in x axis c(angle, h, v)

- xtext:

  Text labels in x axis

- gtext:

  Text labels in groups

- legend:

  the position of legends ("none", "left", "right", "bottom", "top", or
  two-element numeric vector)

- sig:

  Column with the significance

- sigsize:

  Font size in significance letters

- error:

  Show the error bar ("ste" or "std").

- color:

  colored figure c(TRUE, FALSE) or vector with the color.

- opt:

  Add news layer to the plot

## Value

Line o bar plot

## Examples

``` r
 

if (FALSE) { # \dontrun{

library(GerminaR)
library(dplyr)

smr <- ger_summary(SeedN = "seeds"
                   , evalName = "D"
                   , data = prosopis) %>%
  mutate(across(rep:temp, as.factor))
  
av <- aov(grp ~ nacl*temp, smr)

anova(av)

mc <- ger_testcomp(aov = av
                   , comp = c("nacl", "temp"))
                   
plotdt <- mc$table
                    
plot <- fplot(data = plotdt
       , type = "bar"
       , x = "temp"
       , y = "grp"
       , group = "nacl"
       , sig = "sig"
       #, error = "ste"
       , color = T
       , ylimits = c(0, 120, 20)
       )
       
plot
       
} # } 
```
