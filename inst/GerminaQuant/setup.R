# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-10-30
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
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

for (pkg in cran) { 
  if( !require(pkg, character.only = TRUE) ) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } 
}

for (pkg in git) { 
  if( !require(sub(".*/", "", pkg), character.only = TRUE) ) {
    devtools::install_github(pkg)
    library(sub(".*/", "", pkg), character.only = TRUE)
  } 
}  
  
rm(cran, git, pkg)

# -------------------------------------------------------------------------
# references --------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# http://r-pkgs.had.co.nz/release.html

# open https://realfavicongenerator.net/
