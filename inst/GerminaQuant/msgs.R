# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-04-28
# -------------------------------------------------------------------------

suppressPackageStartupMessages({library(GerminaR)})

# -------------------------------------------------------------------------
# message -----------------------------------------------------------------
# -------------------------------------------------------------------------

head <- textcolor(
  "
# -------------------------------------------------------------------------
# ReadMe ------------------------------------------------------------------
# -------------------------------------------------------------------------
  "
)

end <- textcolor(
  "
# -------------------------------------------------------------------------
# GerminaQuant ------------------------------------------------------------
# -------------------------------------------------------------------------
  "
)

message(
  head
  , "\n"
  , textcolor("- ")
  , textcolor("If is the first time running the app consider install the dependencies:")
  , "\n\n"
  , textcolor("> ")
  , textcolor("GerminaR::GerminaQuant(dependencies = TRUE)", "green")
  , "\n\n"
  , textcolor("- ")
  , textcolor("List of dependencies:")
  , "\n"
  , textcolor("> ")
  , textcolor("https://github.com/Flavjack/GerminaR/blob/master/inst/GerminaQuant/pkgs.R", "blue")
  , "\n\n"
  , textcolor("- ")
  , textcolor("More info: ")
  , textcolor("https://flavjack.github.io/GerminaR/", "blue")
  , "\n"
  , end
)

remove(end, head)
