# GerminaR

GerminaR is a platform base in open source R package to calculate and
graphic the main germination indices. GerminaR include a web application
called “GerminQuant for R” for interactive analysis.

## Installation

You can install the released version of GerminaR from
[CRAN](https://cran.r-project.org/package=GerminaR) with:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"GerminaR"``)`

And the development version from
[GitHub](https://github.com/flavjack/GerminaR) with:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"remotes"`](https://remotes.r-lib.org)`)``)`\
`  `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"remotes"``)`\
`remotes``::``install_github``(``"Flavjack/GerminaR"``)`

## GerminaQuant app

\
`GerminaR``::`[`GerminaQuant`](http://germinar.inkaverse.com/reference/GerminaQuant.md)`(``)`

If is the first time running the app you should install the app
dependencies, including the following argument:

\
`GerminaR``::`[`GerminaQuant`](http://germinar.inkaverse.com/reference/GerminaQuant.md)`(``dependencies ``=`` ``TRUE``)`

After install the package and the app dependencies you can access to the
app through the Addins list in Rstudio, or in the following link in the
internet <https://inkaverse.shinyapps.io/germinaquant/>

## Citation

Lozano-Isla, Flavio; Benites-Alfaro, Omar Eduardo; Pompelli, Marcelo
Francisco (2019). GerminaR: An R package for germination analysis with
the interactive web application “GerminaQuant for R.” Ecological
Research, 34(2), 339–346. <https://doi.org/10.1111/1440-1703.1275>
