
#' Summary of Germination Variables
#' 
#' @description This function made a data table with the result of germination variables for each experimental unit.
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param data The name of the data frame containing the data.
#' @return Data frame with the summary values of germination variables.
#' @importFrom dplyr mutate
#' @export

ger_summary <- function(SeedN, evalName, data){
  
  evf <- GerminaR::evalFactor(evalName, data)
  
  gsm <-  dplyr::mutate(evf,
      GRS = ger_GRS(evalName, data), 
      GRP = ger_GRP(SeedN, evalName, data),
      ASG = ger_ASG(SeedN, evalName, data),
      MGT = ger_MGT(evalName, data),
      MGR = ger_MGR(evalName, data),
      GSP = ger_GSP(evalName, data),
      UNC = ger_UNC(evalName, data),
      SYN = ger_SYN(evalName, data),
      VGT = ger_VGT(evalName, data),
      SDG = ger_SDG(evalName, data),
      CVG = ger_CVG(evalName, data)
    )
}




#' Cumulative sum of germination matrix
#' 
#' @description This function made a data table with the cumulative sum of values of germination.
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param method Type of cummulative germination 
#' @param data Data with the germination avaliation process
#' @return Data frame with the cumulative sum
#' @export
ger_cumsum <- function(SeedN, evalName, method = c("percentage", "relative"), data){
  
  
  method <- match.arg(method)
  
  sdn <- data[, SeedN]
  grs <- GerminaR::ger_GRS(evalName, data)
  evf <- GerminaR::evalFactor(evalName, data)
  evd <- GerminaR::evalDays(evalName, data)
  cal <- apply(cbind(evd), 1, cumsum )
  
  if (method == "percentage") {
    
    tmp <- t(cal) * 100 / sdn
    rst <- cbind(evf, tmp)
    rst
    
  }
  
  else if (method == "relative") {
    
    tmp <- t(cal) * 100 / grs
    rst <- cbind(evf, tmp)
    rst
    
  }  
  
}




#' Cumulative sum of germination by period of time for line graphic
#' 
#' @description This function made a data table with the cumulative sum of values of germination by days.
#' @details Need a summary by factor before use it with function SummaryBy.
#' @param Factor Factor wich will be graph in time
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param method Type of cummulative germination 
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @importFrom reshape2 melt
#' @export

ger_intime <- function(Factor, SeedN, evalName, method = c("percentage", "relative"), data){
  
  method <- match.arg(method)
  
  formula <- as.formula(paste( ".", paste( Factor , collapse=" + "), sep=" ~ "))
  smr  <- doBy::summaryBy( formula, data, na.rm = T, keep.names = T)
  

  if (method == "percentage") {
    
    cgr <- GerminaR::ger_cumsum(SeedN, evalName,  method = "percentage", smr)
    evf <- GerminaR::evalFactor(evalName, cgr)
    rsl <- reshape2::melt(cgr, names(evf), na.rm = T)
    
    rsl$variable <- as.factor(gsub("\\D", "", rsl$variable))
    
    rsl
    
  }
  
  else if (method == "relative") {
    
    cgr <- GerminaR::ger_cumsum(SeedN, evalName,  method = "relative", smr)
    evf <- GerminaR::evalFactor(evalName, cgr)
    rsl <- reshape2::melt(cgr, names(evf), na.rm = T)
    
    rsl$variable <- as.factor(gsub("\\D", "", rsl$variable))
    
    rsl
    
    
  }  
  
  
}






