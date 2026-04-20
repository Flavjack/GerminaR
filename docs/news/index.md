# Changelog

## Version 2.1.7

- Link updates

## Version 2.1.6

CRAN release: 2025-10-21

> *Package*

- Now you can collect the data for daily count or cumulative
  germination.
- Improve timeline selection factors

> *GerminaQuant*

- Update GerminaQuant app
- You can include extra variables in the analysis

## Version 2.1.5

CRAN release: 2025-03-05

- *Package*
  - Update app info
  - Full installation from Addins

## Version 2.1.4

CRAN release: 2022-05-18

- *Package*
  - Update logos
  - Update webpage
  - GerminaQuant: column with \[\] are excluded in the analysis

## Version 2.1.3

CRAN release: 2021-10-10

- *Package*
  - Transparent logos
  - Box plot with transparent background
  - New function:
    [`gquant_analysis()`](http://germinar.inkaverse.com/reference/gquant_analysis.md)
- *GerminaQuant*
  - Include model factors manually for any type of experimental design
  - Include the factor comparison selection
  - Include model in the module analysis and plot
  - Include logos

## Version 2.1.2

CRAN release: 2021-06-11

- *Package*
  - [`fplot()`](http://germinar.inkaverse.com/reference/fplot.md)
    - Absolute values to ybrakes
    - Include color using vectors
    - Font size in significance
    - Delete plot borders
  - Added function
    [`textcolor()`](http://germinar.inkaverse.com/reference/textcolor.md)
  - Update logos
- *GerminaQuant*
  - Styled messages in the console
  - Include multiple layers in the graphs `opt`
  - Possibility to exclude the error bar in the plots
  - Include demo videos for the app
  - Change name: Model plot –\> Model diagnostic
  - Update vignettes
    - introduction: add indices info
    - germinaquant: upate module description
  - Include information to
    [`prosopis()`](http://germinar.inkaverse.com/reference/prosopis.md)

## Version 2.1.1

CRAN release: 2021-04-23

Changes incompatible with the old versions. Arguments changed in the
syntax for
[`fplot()`](http://germinar.inkaverse.com/reference/fplot.md).

- *Package*
  - Include app info when run
    [`GerminaQuant()`](http://germinar.inkaverse.com/reference/GerminaQuant.md)
  - Exclude `gtools` from depends
  - Include function
    [`webTable()`](http://germinar.inkaverse.com/reference/webTable.md)
  - Update
    [`ger_boxp()`](http://germinar.inkaverse.com/reference/ger_boxp.md)
  - New domain: germinar.inkaverse.com
  - Update [`fplot()`](http://germinar.inkaverse.com/reference/fplot.md)
    - Allow add new layer.
      e.g. [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
- *GerminaQuant*
  - Suppress error messages in console when run the app
  - Change dependency: `ggpubr` –\> `cowplot`
  - Exclude filter in the app
  - Update tables style
  - Module name change: Outliers -\> Exploratory
  - Update modules:
    - Exploratory
    - Graphics
    - Intime

## Version 2.0.1

CRAN release: 2020-10-25

### Major changes

- Build pkgdown for documentation.
- Update vignettes.

### Bug fixes

- Install dependencies for the app
- Update functions to tidyverse in app

## Version 2.0.0

CRAN release: 2020-09-01

### Major changes

- Reduce the number of dependencies
- Update function
  [`fplot()`](http://germinar.inkaverse.com/reference/fplot.md)
- Release new version of GerminaQuant
- Include diagnostic plots in
  [`ger_testcomp()`](http://germinar.inkaverse.com/reference/ger_testcomp.md)
- Deprecated redundant functions
- Include examples in the functions
- Update vignettes
- Update functions to tidyverse
- Conditional data viewer in the app
- Include GerminaQuant in the Addins list

### Bug fixes

- Fix filters and drop-list in GerminaQuant
- Fix resolution in the graphics produces in GerminaQuant
- Auto close local session of app when finish process

## Version 1.0.0

- Package release
