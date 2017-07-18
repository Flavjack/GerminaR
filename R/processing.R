#' Summary of Germination Variables
#' 
#' @description This function makes a data table with the result of germination variables for each experimental unit.
#' @param SeedN Name of the column with the seed numbers
#' @param evalName Prefix of the evalaution variable
#' @param data The name of the data frame containing the data.
#' @return Data frame with the summary values of germination variables.
#' @importFrom dplyr mutate
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dt <- prosopis
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = dt)
#' smr

ger_summary <- function(SeedN, evalName, data){
  
  data <- as.data.frame(data)
  
  evf <- GerminaR::evalFactor(evalName, data)
  
  gsm <-  dplyr::mutate(evf,
      GRS = ger_GRS(evalName, data), 
      GRP = ger_GRP(SeedN, evalName, data),
      #ASG = ger_ASG(SeedN, evalName, data),
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
     dplyr::summarise(mean = mean(germination), r = n(), std = sd(germination)) %>% 
     dplyr::mutate(ste = std/sqrt(r)) %>% 
     as.data.frame()
   
   rsl$evaluation <- as.numeric(gsub("\\D", "", rsl$evaluation))
   
   rsl
   

}
