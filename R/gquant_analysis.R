#' GerminaQuant data analysis
#' 
#' Function for the analysis module in the app
#'
#' @description Function analysis of variance for summary data.
#' @param data data from ger_summary() function
#' @param response germination indices to analyse
#' @param factors factor as vector or factor model as string
#' @param block block factor for RCBD 
#' @param comparison treatments will be compared.
#' @param type method for made comparison analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return list
#' @export
#' 
#' @examples  
#' 
#' \dontrun{
#' 
#' library(GerminaR)
#' 
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = prosopis)
#' 
#' mc <- gquant_analysis(data = smr
#'                       , response = "grp"
#'                       , factors = c("nacl", "temp")
#'                       , block = "rep"
#'                       , comparison = c("nacl", "temp")
#'                       )
#'                       
#' mc
#' 
#' } 
#' 

gquant_analysis <- function(data
                            , response
                            , factors
                            , block = NA
                            , comparison = NA
                            , type = "snk"
                            , sig = 0.05
                            ){
  
  if(FALSE) {
    
    data <- smr
    type = "snk"
    sig = 0.05
    
    response = "grp"
    factors = c("nacl", "temp")
    block = "rep"
    comparison = c("nacl", "temp")
    
  }
  

# conditions --------------------------------------------------------------
  
  block <- if(is.na(block) | is.null(block) | block == "") {NA} else {block}
  
# data --------------------------------------------------------------------
  
  mfactors <- if(length(factors) == 1) {
    
    factors %>% 
      strsplit(.,'[[:punct:]]+') %>% 
      pluck(1) %>% 
      base::trimws()
    
  } else { factors }

  gdata <- data %>% 
    {if(length({{factors}}) >= 1) mutate(.data = ., across(mfactors, as.factor)) else .} %>% 
    {if(!is.na({{block}})) mutate(.data = ., across({{block}}, as.factor)) else .} %>% 
    mutate(across({{comparison}}, as.factor))
  
# model -------------------------------------------------------------------
  
  model_formula <- if(length(factors) == 1) {
    
    if ( is.na(block) ){
      
      paste(response , factors, sep = " ~ ")
      
    } else if (!is.na(block)) {
      
      paste(response, paste(block, factors, sep = " + "), sep = " ~ ")
      
    }
    
  } else if (length(factors) >= 2) {
    
    if ( is.na(block) ){
      
      paste(response, paste(factors, collapse = "*"), sep = " ~ ")
      
    } else if (!is.na(block)) {
      
      paste(response, paste(block, paste(factors, collapse = "*"), sep = " + "), sep = " ~ ")
      
    }
    
  }
  
# anova -------------------------------------------------------------------
  
  av <- aov(formula(model_formula), data = gdata)

# mean comparison ---------------------------------------------------------
  
  mc <- ger_testcomp(aov = av
                     , comp = comparison
                     , type = type
                     , sig = sig
                     )
  
# parameters --------------------------------------------------------------
  
  parameters <- list(formula = model_formula
                     , response =  response
                     , factors = factors
                     , comparison = comparison
                     , type = type
                     , sig = sig
                     )
# result ------------------------------------------------------------------

  gquant_analysis <- list(data = mc
                          , aov = av
                          , param = parameters
                          )
    
}

