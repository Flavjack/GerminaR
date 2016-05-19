#' @export
runGermiaR <- function() {
  appDir <- system.file("GerminaR", "myapp", package = "GerminaR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GerminaR`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
