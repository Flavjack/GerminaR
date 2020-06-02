#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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

#' Select Evaluation Days
#' 
#' @description Give matrix with the evaluation days
#' @param data Data with germination values
#' @param evalName Prefix of the evaluation variable
#' @return Evaluation columns
#' @importFrom dplyr starts_with select
#' @export
#' @examples 
#' \dontrun{ 
#' library(GerminaR)
#' dt <- prosopis
#' dm <- evalDays(evalName = "D", data = dt)
#' dm
#' }

evalDays <- function(evalName, data){
  
    evd <- data %>% 
      dplyr::select(starts_with(evalName))
    evd

}


#' Select Factors of germination matrix
#' 
#' @description Give matrix with the factor
#' @param evalName Prefix of the evaluation variable
#' @param data Data with germination values
#' @return Factor columns
#' @importFrom dplyr starts_with select
#' @export
#' @examples 
#' \dontrun{ 
#' library(GerminaR)
#' dt <- prosopis
#' dm <- evalFactor(evalName = "D", data = dt)
#' dm
#' }

evalFactor <- function(evalName, data){
  
  evf <- data %>% 
    dplyr::select(-starts_with(evalName))
  
  evf
  
}

