# Version 2.1.3

- *Package*
  - Transparent logos
  - Box plot with transparent background
  
- *GerminaQuant*
  - Include model factors manually for any experimental design
  

# Version 2.1.2

- *Package*
  - `fplot()` 
    - Absolute values to ybrakes
    - Include color using vectors
    - Font size in significance
    - Delete plot borders
  - Added function `textcolor()`
  - Update logos
  
- *GerminaQuant*
  - Styled messages in the console
  - Include multiple layers in the graphs `opt`
  - Possibility to exclude the error bar in the plots
  - Include demo videos for the app
  - Change name: Model plot --> Model diagnostic
  - Update vignettes
    - introduction: add indices info
    - germinaquant: upate module description
  - Include information to `prosopis()`
  
# Version 2.1.1

Changes incompatible with the old versions. Arguments changed in the syntax for `fplot()`.

- *Package*
  - Include app info when run `GerminaQuant()`
  - Exclude `gtools` from depends
  - Include function `webTable()`
  - Update `ger_boxp()`
  - New domain: germinar.inkaverse.com
  - Update `fplot()`
    - Allow add new layer. e.g. `coord_flip()`

- *GerminaQuant*
  - Suppress error messages in console when run the app
  - Change dependency: `ggpubr` --> `cowplot`
  - Exclude filter in the app
  - Update tables style
  - Module name change: Outliers -> Exploratory
  - Update modules:
    - Exploratory
    - Graphics
    - Intime

# Version 2.0.1

## Major changes

- Build pkgdown for documentation.
- Update vignettes.

## Bug fixes

- Install dependencies for the app
- Update functions to tidyverse in app

# Version 2.0.0

## Major changes

- Reduce the number of dependencies
- Update function `fplot()`
- Release new version of GerminaQuant
- Include diagnostic plots in `ger_testcomp()`
- Deprecated redundant functions
- Include examples in the functions
- Update vignettes
- Update functions to tidyverse
- Conditional data viewer in the app
- Include GerminaQuant in the Addins list

## Bug fixes

- Fix filters and drop-list in GerminaQuant
- Fix resolution in the graphics produces in GerminaQuant
- Auto close local session of app when finish process

# Version 1.0.0

- Package release
