
#' Summary of Germination Variables
#' @description This function made a data table with the result of germination variables for each experimental unit.
#' @param data Data with the germination avaliation process
#' @return Data frame with the values of germination variables
#' @export
ger_summary <- function(data){
  SDN <- data$SDN
  eval_days <- evalDays(data)
  sm <- data %>% evalFact() %>% 
    dplyr::mutate(
      GRS = ger_GRS(eval_days), 
      GRP = ger_GRP(SDN,eval_days),
      ASG = ger_ASG(SDN,eval_days),
      MGT = ger_MGT(eval_days),
      GRU = ger_GRU(eval_days),
      GSI = ger_GSI(eval_days),
      CVL = ger_CVL(eval_days),
      MGR = ger_MGR(eval_days),
      VGT = ger_VGT(eval_days),
      SDG = ger_SDG(eval_days),
      CVG = ger_CVG(eval_days)
    ) 
}


#' Cumulative sum of germination matrix
#' @description This function made a data table with the cumulative sum of values of germination.
#' @param data Data with the germination avaliation process
#' @return Data frame with the cumulative sum
#' @export
ger_cumsum <- function(data){
  
  fc <- evalFact(data)
  eval_days <- evalDays(data)
  tmp <- apply(cbind(eval_days), 1, cumsum)
  rst <- t(tmp)
  cbind(fc, rst)
  
}


#' Cumulative sum of germination by period of time for line graphic
#' @description This function made a data table with the cumulative sum of values of germination by days.
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @export
ger_day <- function(data){
 evalFactor <- evalFact(data)
 df <- reshape::melt(data, names(evalFactor))
 df
}

