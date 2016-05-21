
#' Summary of Germination Variables
#' @description This function made a data table with the result of germination variables for each experimental unit.
#' @param data Data with the germination avaliation process
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @return Data frame with the values of germination variables
#' @importFrom dplyr mutate '%>%'
#' @export
ger_summary <- function(data, SeedN, evalName){
  SeedN <- data$SDN
  evalDays <- evalDays(data, evalName)
  
  sm <- GerminaR::evalFactor(data, evalName) %>% 
    dplyr::mutate(
      GRS = ger_GRS(evalDays), 
      GRP = ger_GRP(SeedN,evalDays),
      ASG = ger_ASG(SeedN,evalDays),
      MGT = ger_MGT(evalDays),
      GRU = ger_GRU(evalDays),
      GSI = ger_GSI(evalDays),
      CVL = ger_CVL(evalDays),
      MGR = ger_MGR(evalDays),
      VGT = ger_VGT(evalDays),
      SDG = ger_SDG(evalDays),
      CVG = ger_CVG(evalDays)
    )
}


#' Cumulative sum of germination matrix
#' @description This function made a data table with the cumulative sum of values of germination.
#' @param data Data with the germination avaliation process
#' @return Data frame with the cumulative sum
#' @export
ger_cumsum <- function(data){
  
  fc <- GerminaR::evalFactor(data)
  evalDays <- GerminaR::evalDays(data)
  tmp <- apply(cbind(evalDays), 1, cumsum)
  rst <- t(tmp)
  cbind(fc, rst)
  
}


#' Cumulative sum of germination by period of time for line graphic
#' @description This function made a data table with the cumulative sum of values of germination by days.
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @importFrom reshape melt
#' @export
ger_day <- function(data){
  evalFactor<- evalFactor(data)
 df <- reshape::melt(data, names(evalFactor))
 df
}

