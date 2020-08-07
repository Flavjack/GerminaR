#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

utils::globalVariables(c("."))

#' Repeated Rows in a data matrix
#' 
#' @description This function made a data table with the evaluation days of germination
#' @param Rseq Row sequence for the data matrix
#' @param Nrow Number of rows for the data matrix
#' @return Data Matrix with day of the germination
#' @export

rep_row <- function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}
