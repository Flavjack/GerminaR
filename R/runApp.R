#'  @description When the function is invoked, GerminaR display a shiny app application showing 
#'  web tabs about import, check, analyze and export germination data. 
#' @family GerminaR
#' @importFrom shiny runApp
#' @export
runGerminaQuant <- function() {
  appDir <- system.file("GerminaR", "myapp", package = "GerminaR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GerminaR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
