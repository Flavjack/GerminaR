#' GerminaQuant data analysis
#' 
#' Function for the analysis module in the app
#'
#' @description Function analysis of variance for summary data.
#' @param data data from ger_summary() function
#' @param response germination indices to analyse
#' @param factors factor as vector or factor model as string
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
#' library(dplyr)
#' data <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = data)
#' 
#' mc <- gquant_analysis(data = smr
#'                       , response = "grp"
#'                       , factors = c("nacl", "temp")
#'                       , block = "rep"
#'                       , comparison = c("nacl", "temp")
#'                       )
#'                       
#' mc$param$formula
#'                       
#' 
#' } 

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
    block  <- NA
    response <- "grp"
    factors <- "block + temp * nacl"
    factors <- c("nacl", "temp")
    comparison <- c("temp", "nacl")
    type = "snk"
    sig = 0.05
    
  }
  

# conditions --------------------------------------------------------------
  
  block <- if(is.na(block) | is.null(block) | block == "") {NA} else {block}
  
# data --------------------------------------------------------------------
  
  mfactors <- if(length(factors) == 1) factors %>% 
    strsplit(.,'[[:punct:]]+') %>% 
    pluck(1) %>% 
    base::trimws()

  data <- data %>% 
    {if(length(factors) == 1) mutate(.data = ., across(mfactors, as.factor)) else .} %>% 
    {if(!is.na(block)) mutate(.data = ., across(block, as.factor)) else .} %>% 
    mutate(across({{comparison}}, as.factor))
  
# model -------------------------------------------------------------------
  
  model_formula <- if(length(factors) == 1) {
    
    as.formula(paste(response , factors, sep = " ~ "))
    
  } else if (length(factors) >= 2) {
    
    if ( is.na(block) ){
      
      as.formula(paste(response, paste(factors, collapse = "*"), sep = " ~ "))
      
    } else if (!is.na(block)) {
      
      as.formula(paste(response, paste(block, paste(factors, collapse = "*"), sep = " + "), sep = " ~ "))
      
    }
    
  }
  
# anova -------------------------------------------------------------------
  
  av <- aov(formula = model_formula, data = data)

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

