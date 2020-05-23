#' Mean Comparison Table Summary
#' 
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs. 
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @importFrom dplyr mutate select rename_ group_by_ summarise full_join rename
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#' @export

dtsm <- function(meanComp){
  
  #to avoid no bisible global variable function
  std <- r <- trt <- mean <- Min <- Max <- ste <- groups <- NULL
  
  fct <- as.character(meanComp$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))
  
  dtmn <- meanComp$means %>% rename_(mean = names(meanComp$means[1]))
  
  dtgr <- meanComp$groups %>% rownames_to_column(var = "trt")
  
  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))
  
  dta <- dtmn %>% 
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn))) 
  
  sm <- dplyr::full_join(dta, dtgr, by = "trt") %>% 
    dplyr::select(trt, mean, Min, Max, r, std, ste, groups) %>% 
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>% 
    dplyr::rename(min = Min, max = Max, sg = groups)
  
}


#' Multiple comparison test
#'
#' @description Function analisis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparision analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return Table with complete data for graphics
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @export


ger_testcomp <- function( aov, comp, type = "snk", sig = 0.05){
  
  if( type == "snk"){
    
    mc <- agricolae::SNK.test(y = aov, trt = comp, alpha = sig)
    
  } else if (type == "tukey"){
    
    mc <- agricolae::HSD.test(y = aov, trt = comp, alpha = sig)
    
  } else if (type == "duncan"){
    
    mc <- agricolae::duncan.test(y = aov, trt = comp, alpha = sig)
    
  }
  
  GerminaR::dtsm(mc)
  
}