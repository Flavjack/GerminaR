#' Summary of Germination indices
#' 
#' @description This function makes a data table with the result of germination indices for each experimental unit.
#' @param SeedN Name of the column with the seed numbers
#' @param evalName Prefix of the evaluation variable
#' @param data The name of the data frame containing the data.
#' @return Data frame with the summary values of germination variables.
#' @importFrom dplyr mutate
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' data <- prosopis
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = data)
#' smr

ger_summary <- function(SeedN, evalName, data){
  
  evf <- data %>% 
    select(!starts_with({{evalName}})) 
  
  gsm <- mutate( evf,
      grs = ger_GRS(evalName, data), 
      grp = ger_GRP(SeedN, evalName, data),
      mgt = ger_MGT(evalName, data),
      mgr = ger_MGR(evalName, data),
      gsp = ger_GSP(evalName, data),
      unc = ger_UNC(evalName, data),
      syn = ger_SYN(evalName, data),
      vgt = ger_VGT(evalName, data),
      sdg = ger_SDG(evalName, data),
      cvg = ger_CVG(evalName, data)
      )
  
# result ------------------------------------------------------------------

  return(gsm)
  
}
