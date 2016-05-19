
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
#' @return Evalution colums
#' @export
evalDays <- function(data){
    select(data, starts_with("D"))
}


#' Select Factors of germination matrix
#' @description Give matrix with the factor
#' @param data Data with germination values
#' @return Factor colums
#' @export
evalFact <- function(data){
  select(data, -starts_with("D"))
}