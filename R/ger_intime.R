#' Cumulative sum of germination by period of time for line graphic
#' 
#' @description This function makes a data table with the cumulative sum of values of germination by days.
#' @details Need a summary by factor before use it with function SummaryBy.
#' @param Factor Factor which will be graph in time
#' @param SeedN Name of the column with the seed numbers
#' @param evalName Prefix of the evaluation variable
#' @param method Type of cumulative germination. "percentage" or "relative" 
#' @param data Data with the germination evaluation process
#' @return Data frame with the germination by period
#' @importFrom stats sd
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' 
#' library(GerminaR)
#' data <- prosopis
#' grt <- ger_intime(Factor = "nacl"
#'                   , SeedN = "seeds"
#'                   , evalName = "D"
#'                   , method = "relative"
#'                   , data = data
#'                   )
#' head(grt, 10)
#'  
#' fplot(data = grt
#'       , type = "line"
#'       , x = "evaluation"
#'       , y = "mean"
#'       , groups = "nacl"
#'       , sig = NULL
#'       ) 

ger_intime <- function(Factor
                       , SeedN
                       , evalName
                       , method = "percentage"
                       , data
                       ){
    
   n <- std <- r <- germination <- evaluation <- where <- NULL  
   
# arguments ---------------------------------------------------------------
   
   method <- match.arg(method, c("percentage", "relative"))

   evf <- data %>% select(!starts_with({{evalName}})) 
   evd <- data %>% select(starts_with({{evalName}}))
   sdn <- data[, SeedN]
   
# Cumulative --------------------------------------------------------------
# -------------------------------------------------------------------------
   
   grs <- ger_GRS(evalName, data)
   cal <- apply(cbind(evd), 1, cumsum )
   
   if (method == "percentage") {
     
     temp <- t(cal) * 100 / sdn
     acum <- cbind(evf, temp)
     
   } else if (method == "relative") {
     
     temp <- t(cal) * 100 / grs
     acum <- cbind(evf, temp)
     
   }  
   
# in-time -----------------------------------------------------------------
# -------------------------------------------------------------------------
   
   git <- acum %>% 
      pivot_longer(names(evd)
                  , names_to = "evaluation"
                  , values_to = "germination"
                  ) %>% 
      group_by(.data[[Factor]], evaluation) %>% 
      summarise(mean = mean(germination)
               , r = dplyr::n()
               , std = sd(germination)
               , min = min(germination)
               , max = max(germination)
               ) %>% 
      ungroup() %>% 
      mutate(ste = std/sqrt(r)) %>% 
      mutate(evaluation = gsub("\\D", "", evaluation)) %>% 
      mutate(across(evaluation, ~ as.numeric(.))) %>% 
      arrange(evaluation) %>% 
      mutate(across( {{Factor}},  ~ as.factor(.))) %>% 
      mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
      mutate(across(where(is.numeric), ~replace(., is.nan(.), 0)))

# result ------------------------------------------------------------------
   
   git

}
