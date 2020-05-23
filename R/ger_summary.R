#' Summary of Germination indices
#' 
#' @description This function makes a data table with the result of germination indices for each experimental unit.
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
  
  evf <- evalFactor(evalName, data)
  
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