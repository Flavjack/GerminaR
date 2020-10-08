#' GerminaQuant for R
#'
#' GermiQuant for R app allows make the calculation for the germination indices incredibly easy in a interactive applications build in base a GerminaR R package and Shiny.
#' GermiQuant app is live!. Outputs change instantly as users modify inputs, without requiring a reload the app.
#' The principal features of the application allow calculate the principal germination indices, statistical analysis and easy way to plot the results.
#' 
#' @param dependencies Install package dependencies for run the app
#' 
#' @source Lozano-Isla, F., Benites Alfaro, O., & Pompelli, M. F. (2016). GerminaQuant for R (Patent No. BR 51 2016 001327-3). https://flavjack.shinyapps.io/germinaquant/
#' 
#' @family GerminaR
#' @importFrom shiny runApp
#' @export

GerminaQuant <- function(dependencies = FALSE) {
  
  if (dependencies == TRUE ) {
    
    source("https://raw.githubusercontent.com/Flavjack/GerminaR/master/inst/GerminaQuant/setup.R")
    
  }
  
  appDir <- system.file("GerminaQuant", package = "GerminaR")
  
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GerminaR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
