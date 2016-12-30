#'  @description GermiQuant App allows make the calculation for the germination variables incredibly easy in a interactive applications build in base a GerminaR R package and Shiny.
#'  GermiQuant App is live!. Outputs change instantly as users modify inputs, without requiring a reload the app.
#'  The principal features of the application allow calculate the principal germination variables, statistical analysis and easy way to plot the results.
#' @family GerminaR
#' @importFrom shiny runApp
#' @export
GerminaQuant <- function() {
  appDir <- system.file("GerminaQuant", package = "GerminaR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GerminaR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
