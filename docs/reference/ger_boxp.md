# Boxplot graphic

Function use the raw data for made a boxplot graphic

## Usage

``` r
ger_boxp(
  data,
  x,
  y,
  group = NULL,
  xlab = NULL,
  ylab = NULL,
  glab = NULL,
  ylimits = NULL,
  xrotation = NULL,
  legend = "top",
  xtext = NULL,
  gtext = NULL,
  opt = NULL
)
```

## Arguments

- data:

  raw data

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

  Limitis and break of the y axis c(init, end, brakes)

- xrotation:

  Rotation in x axis c(angle, h, v)

- legend:

  the position of legends ("none", "left", "right", "bottom", "top", or
  two-element numeric vector)

- xtext:

  Text labels in x axis

- gtext:

  Text labels in groups

- opt:

  Add news layer to the plot

## Value

boxplot

## Examples

``` r
if (FALSE) { # \dontrun{

library(GerminaR)

fb <- ger_summary(SeedN = "seeds", evalName = "D", data = prosopis)

ger_boxp(data = fb
         , x =  "nacl"
         , y = "grp"
         , group = "temp"
         )

} # }
```
