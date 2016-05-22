
#' Summary of Germination Variables
#' @description This function made a data table with the result of germination variables for each experimental unit.
#' @param data Data with the germination avaliation process
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @return Data frame with the values of germination variables
#' @export
ger_summary <- function(data, SeedN, evalName){
  SeedN <- data$SDN # Revisar el valor por defecto
  evalDays <- evalDays(data, evalName)
  
  sm <- evalFactor(data, evalName) %>% 
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
#' @param evalName Prefix of the evalaution variable
#' @param SeedN Name of the colonn with the seed numbers
#' @return Data frame with the cumulative sum
#' @export
ger_cumsum <- function(data, evalName, SeedN){
  
  evalFactor <- evalFactor(data, evalName)
  evalDays <- evalDays(data, evalName)

  SeedN <- data$SDN # Revisar el valor por defecto
  
  tmp <- apply(cbind(evalDays), 1, cumsum )
  tmp2 <- tmp * (100/SeedN)
  
  rst <- t(tmp2)
  
  cbind(evalFactor, rst)
  
}


#' Cumulative sum of germination by period of time for line graphic
#' @description This function made a data table with the cumulative sum of values of germination by days.
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @export
ger_day <- function(data, evalName){
  cger <- ger_cumsum(data, evalName)
  evalFactor<- evalFactor(cger, evalName)
  df <- reshape::melt(cger, names(evalFactor), na.rm = T)

}






