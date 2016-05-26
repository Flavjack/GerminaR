
#' Repeated Rows in a data matrix
#' 
#' @description This function made a data table with the evaluation days of germination
#' @param Rseq Row sequance for the data matrix
#' @param Nrow Number of rows for the data matrix
#' @return Data Matrix with day of the germination
rep.row<-function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}


#' Select colum according the initial letters of the words
#' 
#' @description Select colum according the initial letters of the words
#' @param vars vars
#' @param match match 
#' @param ignore.case case
#' @return Matrix with the select colum
#' @author Hadley Wickham
#' @details https://github.com/hadley/dplyr/blob/50309db8f04cbcc87e4568a4bfa1f0c718e824c9/R/select-utils.R
#' @export
starts_with <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)
  
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)
  
  if (ignore.case) vars <- tolower(vars)
  which(substr(vars, 1, n) == match)
}



#' Select Evaluation Days
#' 
#' @description Give matrix with the evaluation days
#' @param data Data with germination values
#' @param evalName Prefix of the evalaution variable
#' @return Evalution colums
#' @importFrom dplyr select
#' @export
evalDays <- function(evalName, data){
  
    evd <- dplyr::select(data, starts_with(evalName))
    evd

}


#' Select Factors of germination matrix
#' 
#' @description Give matrix with the factor
#' @param data Data with germination values
#' @param evalName Prefix of the evalaution variable
#' @return Factor colums
#' @importFrom dplyr select
#' @export
evalFactor <- function(evalName, data){
  
  evf <- dplyr::select(data, -starts_with(evalName))
  
  evf[,colnames(evf)] <- lapply(evf[,colnames(evf)] , as.factor)
  
  evf
  
}







