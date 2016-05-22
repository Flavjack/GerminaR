
#' Summary of Germination Variables
#' 
#' @description This function made a data table with the result of germination variables for each experimental unit.
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param freq Frecuency of evualtion of the experimental units (days or hour)
#' @param data The name of the data frame containing the data.
#' @return Data frame with the summary values of germination variables.
#' @importFrom dplyr mutate '%>%'
#' @export

ger_summary <- function(SeedN, evalName, freq = 1, data){
  sm <- GerminaR::evalFactor(evalName, data) %>% 
    dplyr::mutate(
      GRS = ger_GRS(evalName, data), 
      GRP = ger_GRP(SeedN, evalName, data),
      ASG = ger_ASG(SeedN, evalName, data),
      MGT = ger_MGT(evalName, freq = 1, data),
      MGR = ger_MGR(evalName, freq = 1, data),
      CVL = ger_CVL(evalName, freq = 1, data),
      GRU = ger_GRU(evalName, data),
      GSI = ger_GSI(evalName, data),
      VGT = ger_VGT(evalName, freq = 1, data),
      SDG = ger_SDG(evalName, freq = 1, data),
      CVG = ger_CVG(evalName, freq = 1, data)
    )
}



#' Cumulative sum of germination matrix
#' 
#' @description This function made a data table with the cumulative sum of values of germination.
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param data Data with the germination avaliation process
#' @return Data frame with the cumulative sum
#' @export
ger_cumsum <- function(SeedN, evalName, data){
  
  sdn <- data[, SeedN]
  evalFactor <- GerminaR::evalFactor(evalName, data)
  evalDays <- GerminaR::evalDays(evalName, data)
  cal <- apply(cbind(evalDays), 1, cumsum )
  tmp <- cal * (100/sdn)
  rst <- t(tmp)
  cbind(evalFactor, rst)

}


#' Cumulative sum of germination by period of time for line graphic
#' 
#' @description This function made a data table with the cumulative sum of values of germination by days.
#' @details Need a summary by factor before use it with function SummaryBy. 
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @importFrom reshape melt
#' @export

ger_intime <- function(SeedN, evalName, data){

  cgr <- ger_cumsum(SeedN, evalName, data)
  evf <- evalFactor(evalName, cgr)
  rsl <- reshape::melt(cgr, names(evf), na.rm = T)
  rsl

}






