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


#' Transform dataframe based in a dictionary
#'
#' @description Transfor all fieldbook data frame according to data dictionary
#' @param fieldbook data frame with the original information
#' @param dictionary data frame with at least 3 colums (original names, new names and variable type)
#' @param from Column name of a data frame with the original names of the variables
#' @param to Column name of a data frame with the new names for the variables
#' @param index Column name of a data frame with the type and level of the variables
#' @param colnames Character vector with the column names
#' @return List with original fieldbook, variables and new fieldbook
#' @export

metamorphosis <- function(fieldbook, dictionary, from, to, index, colnames){
  
  library(tidyverse)
  library(googlesheets4)
  
  # Import dictionary -------------------------------------------------
  
  dictionary_used <- dictionary %>% 
    drop_na(from) %>% 
    mutate_at(vars(from, to), as.character) 
  
  # column names ------------------------------------------------------------
  
  rename_colums <- function(fieldbook, dictionary, from, to, index, colnames) {
    
    
    cln <- dictionary %>%
      dplyr::filter(!!sym(index) %in% colnames) 
    
    # Varible levels ----------------------------------------------------------
    
    vrl <- dictionary %>% 
      dplyr::filter(!(!!sym(index)) %in% colnames) 
    
    # Change colnames in the fieldbook ----------------------------------------
    
    old <- cln %>% 
      select(from) %>% 
      as_vector()
    
    new <- cln %>% 
      select(to) %>% 
      as_vector()
    
    fbr <- fieldbook %>% 
      rename_at(vars(old), ~ new)  
    
    fbr
    
  }
  
  fb_renamed <- rename_colums(fieldbook, dictionary_used, from, to, index, colnames)
  
  # Recode the variable levels ----------------------------------------------
  
  rename_levels <- function(fb_renamed, dictionary, from, to, index, colnames, variable){
    
    # variable levels
    
    vrl <- dictionary %>% 
      dplyr::filter(!(!!sym(index)) %in% colnames) 
    
    
    # Check if variable exist
    
    colums_to_recode <- dictionary %>% 
      dplyr::filter(!(!!sym(index)) %in% colnames) %>% 
      select(index) %>% 
      unique() %>% 
      as_vector()
    
    
    if(is.element(variable, colums_to_recode) == FALSE){ 
      
      rnf <- fb_renamed %>% 
        select(variable)
      
      rnf
      
    } else { 
      
      # Old variable names
      
      old_v <-  vrl  %>%
        filter(!!sym(index) == variable) %>% 
        select(from) %>%
        as_vector()
      
      # New variable names
      
      new_v <-  vrl %>%
        filter(!!sym(index) == variable) %>% 
        select(to) %>%
        as_vector()
      
      # Lista variables to recode
      
      rnm <- structure(as.character(new_v),
                       names = as.character(old_v))
      
      # Recode one variable
      
      rnf <- fb_renamed %>%
        mutate_at(variable, list(~recode(., !!!rnm))) %>% 
        select(variable)
      
      rnf 
      
    }
    
    
  }
  
  # Recode all the variables in the data frame
  
  fb_recoded <- lapply(1:ncol(fb_renamed), function(x) {
    
    fb_renamed %>% 
      rename_levels(., dictionary_used, from, to, index, colnames, variable = colnames(.)[x])
    
  })
  
  # Unite the lists ---------------------------------------------------------
  
  fb_mutated <- do.call(cbind, fb_recoded) %>% as_tibble()
  
  
  # Extract used dictionary -------------------------------------------------
  
  vrl_used <- dictionary %>% 
    drop_na(from) %>% 
    filter(!!sym(index) %in% colnames) %>% 
    select(to) %>% 
    unique() %>% 
    as_vector()
  
  lvl_used <- dictionary %>% 
    filter(!!sym(index) %in% c(vrl_used)) %>% 
    select(to) %>% 
    unique() %>% 
    as_vector()
  
  
  dictionary_all <- dictionary %>% 
    filter(!!sym(to) %in% c(vrl_used, lvl_used)) %>% 
    mutate_at(vars(from, to), as.character) %>% 
    distinct(!!sym(from), !!sym(to), .keep_all = TRUE) %>% 
    distinct(!!sym(to), .keep_all = TRUE)



  # Result ------------------------------------------------------------------
  
  list(
    dictionary = dictionary_all,
    fieldbook = fb_mutated
  )
  
}
