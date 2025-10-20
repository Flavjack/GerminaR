#' Summary of Germination indices
#' 
#' @description This function makes a data table with the result of germination indices for each experimental unit.
#' @param factors Factor included for the analysis.
#' @param cumulative Type of data collection `[logic: FALSE or TRUE]`
#' @param SeedN Name of the column with the seed numbers.
#' @param evalName Prefix of the evaluation variable.
#' @param data The name of the data frame containing the data.
#' @return Data frame with the summary values of germination variables.
#' @import dplyr
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' fb <- prosopis
#' smr <- ger_summary(factors = c("nacl", "temp", "rep")
#'                    , SeedN = "seeds"
#'                    , evalName = "D"
#'                    , cumulative = FALSE
#'                    , data = fb)
#' smr
#' 

ger_summary <- function(factors, SeedN, evalName, cumulative = FALSE, data){
  
if (FALSE) {
  
  data <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1QziIXGOwb8cl3GaARJq6Ez6aU7vND_UHKJnFcAKx0VI/edit?gid=800931632#gid=800931632")
  SeedN = "seeds"
  evalName = "ger_"
  cumulative = TRUE
  
  factors <- c("Inoculant", "peg", "block")
  
}
  
  if (is.null(factors) || length(factors) == 0 || any(is.na(factors) | factors == "")) {
    stop("Need to select factors")
  }
  
  gerdt <- data %>% 
    mutate(across(starts_with({{evalName}}), ~ as.numeric(.))) %>% 
    mutate(across({{factors}}, ~ as.factor(.))) %>% 
    as.data.frame()
  
  # str(dtx)
  
  gernames <- gerdt %>% 
    dplyr::select(starts_with({{evalName}})) %>% 
    names()
  
  dtx <- if(isTRUE(cumulative)) {
    
    gerdt %>% 
      dplyr::select(starts_with({{evalName}})) %>% 
      rowwise() %>% 
      dplyr::summarise(.data[[gernames[1]]]
                       , d = list(diff(c_across(everything())))) %>% 
      tidyr::unnest_wider(.data$d, names_sep="", names_repair=\(x) gernames) %>% 
      cbind(gerdt %>% dplyr::select(!starts_with(evalName)), .) 
    
  } else { gerdt }
  
  ger_eval <- dtx %>% 
    dplyr::select(starts_with({{evalName}}))
  
  if (any(ger_eval < 0, na.rm = TRUE)) {
    
    print(ger_eval)
    
    stop("Negative values in the germination count")
  }
  
  info <- dtx %>% 
    select(!starts_with({{evalName}})) 

  gsm <- mutate(info,
      grs = ger_GRS(evalName, dtx), 
      grp = ger_GRP(SeedN, evalName, dtx),
      mgt = ger_MGT(evalName, dtx),
      mgr = ger_MGR(evalName, dtx),
      gsp = ger_GSP(evalName, dtx),
      unc = ger_UNC(evalName, dtx),
      syn = ger_SYN(evalName, dtx),
      vgt = ger_VGT(evalName, dtx),
      sdg = ger_SDG(evalName, dtx),
      cvg = ger_CVG(evalName, dtx)
      ) 
  
  # str(gsm)
  
  if (any(gsm$grp > 100, na.rm = TRUE)) {
    
    stop("Germination more than 100%. Please check your data")
  }
  
# result ------------------------------------------------------------------

  return(gsm)
  
}
