#' Transform dataframe based in a dictionary
#'
#' @description Transform all fieldbook data frame according to data dictionary
#' @param fieldbook data frame with the original information
#' @param dictionary data frame with at least 3 columns (original names, new names and variable type)
#' @param from Column name of a data frame with the original names of the variables
#' @param to Column name of a data frame with the new names for the variables
#' @param index Column name of a data frame with the type and level of the variables
#' @param colnames Character vector with the column names
#' @return List with original fieldbook, variables and new fieldbook
#' @importFrom dplyr mutate_all starts_with sym vars filter rename_at mutate_at 
#' @importFrom purrr as_vector 
#' @importFrom tidyr drop_na as_tibble
#' @export

metamorphosis <- function(fieldbook, dictionary, from, to, index, colnames){
  
  # Import dictionary -------------------------------------------------
  
  dictionary_used <- dictionary %>% 
    drop_na(from) %>% 
    mutate_all(as.character) 
  
  # column names ------------------------------------------------------------
  
  rename_colums <- function(fieldbook, dictionary, from, to, index, colnames) {
    
    
    cln <- dictionary %>%
      dplyr::filter(!!sym(index) %in% colnames) %>% 
      select(from, to) %>% 
      drop_na(from)
    
    old_names <- cln %>% 
      select(from) %>%  
      as_vector()
    
    new_names <- cln %>% 
      select(to) %>% 
      as_vector()
    
    fbr <- fieldbook %>% 
      rename_at(vars(old_names), ~ new_names)  
    
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
  
  fb_recoded <- do.call(cbind, fb_recoded) %>% as_tibble()
  
  
  # Extract used dictionary -------------------------------------------------
  
  vrl <- dictionary %>% 
    drop_na(from) %>% 
    filter(!!sym(index) %in% colnames) %>% 
    select(to) %>% 
    unique() %>% 
    unlist() %>% 
    as_vector()
  
  
  dictionary_all <- dictionary %>% 
    mutate_all(as.character) %>% 
    filter(!!sym(to) %in% vrl | !!sym(index) %in% vrl)
  
  # Result ------------------------------------------------------------------
  
  list(
    dictionary = dictionary_all,
    fieldbook = fb_recoded
  )
  
}
