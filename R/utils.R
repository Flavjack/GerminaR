
#' Repeated Rows in a data matrix
#' @description This function made a data table with the evaluation days of germination
#' @param Rseq Row sequance for the data matrix
#' @param Nrow Number of rows for the data matrix
#' @return Data Matrix with day of the germination
rep.row<-function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}


#' Select Evaluation Days
#' @description Give matrix with the evaluation days
#' @param data Data with germination values
#' @param evalName Prefix of the evalaution variable
#' @return Evalution colums
#' @export
evalDays <- function(data , evalName){
    dplyr::select(data, starts_with(evalName))
}


#' Select Factors of germination matrix
#' @description Give matrix with the factor
#' @param data Data with germination values
#' @param evalName Prefix of the evalaution variable
#' @return Factor colums
#' @export
evalFactor <- function(data, evalName){
  dplyr::select(data, -starts_with(evalName))
}