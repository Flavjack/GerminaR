# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/germinaquant/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c("devtools" 
          , "shiny"
          , "metathis"
          , "tidyverse"
          , "shinydashboard"
          , "shinyWidgets"
          , "gsheet"
          , "readxl"
          , "DT"
          , "ggpubr"
          )

git <- c(
  "Flavjack/GerminaR"
  )

installed <- c(cran, sub(".*/", "", git)) %in% rownames(installed.packages())

if (any(installed == FALSE)) {
  cran_missing <- cran %in% c(cran, sub(".*/", "", git))[!installed == TRUE]
  cran_install <- c(cran, sub(".*/", "", git))[cran_missing == TRUE]
  install.packages( cran_install )
}

invisible(lapply(sub(".*/", "", git), unloadNamespace))
invisible(lapply(git, devtools::install_github, dependencies = TRUE))
invisible(lapply(c(cran, sub(".*/", "", git)), library, character.only = TRUE))
rm(cran, git, installed)

# References .:

# open https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# http://r-pkgs.had.co.nz/release.html

# open https://realfavicongenerator.net/
