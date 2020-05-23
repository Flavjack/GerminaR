#' Cumulative sum of germination matrix
#' 
#' @description This function makes a data table with the cumulative sum of values of germination.
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param method Type of cummulative germination. "percentage" or "relative" 
#' @param data Data with the germination avaliation process
#' @return Data frame with the cumulative sum
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dt <- prosopis
#' gcs <- ger_cumsum(SeedN = "seeds", evalName = "D", method = "percentage", data = dt)
#' head(gcs, 10)


ger_cumsum <- function(SeedN, evalName, method = "percentage", data){
  
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
#' @description This function makes a data table with the cumulative sum of values of germination by days.
#' @details Need a summary by factor before use it with function SummaryBy.
#' @param Factor Factor wich will be graph in time
#' @param SeedN Name of the colonn with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param method Type of cummulative germination. "percentage" or "relative" 
#' @param data Data with the germination avaliation process
#' @return Data frame with the germination by period
#' @importFrom stats sd
#' @export
#' @examples
#' 
#' library(GerminaR)
#' dt <- prosopis
#' grt <- ger_intime(Factor = "nacl", SeedN = "seeds", 
#'                   evalName = "D", method = "percentage", data = dt)
#' head(grt, 10)

ger_intime <- function(Factor, SeedN, evalName, method = "percentage", data){
    
   n <- std <- r <- germination <- NULL  #To avoid NOTE: ger_intime: no visible binding for global variable
   
   data <- as.data.frame(data)
   
   grt <- ger_cumsum(SeedN, evalName, method, data)
   
   evd <- GerminaR::evalDays(evalName, grt)
   
   git <- tidyr::gather_(data = grt, 
                         key = "evaluation",
                         value = "germination",
                         names(evd), na.rm = TRUE)
   
   
   
   rsl <- git %>% 
     dplyr:: group_by_(Factor, "evaluation") %>% 
     dplyr::summarise(mean = mean(germination), r = dplyr::n(), std = sd(germination)) %>% 
     dplyr::mutate(ste = std/sqrt(r)) %>% 
     as.data.frame()
   
   rsl$evaluation <- as.numeric(gsub("\\D", "", rsl$evaluation))
   
   rsl
   

}


