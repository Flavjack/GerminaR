#' Calculation of the variables data matrix
#' @description This function Calculate the Number of Germinated Seeds
#' @param fieldbook Data frame with the germination evaluation
#' @author Omar Benites
#' @return Names of the evaluation days
#' @export

germcalculate <- function(fieldbook){
  
  seeds <- fieldbook$SDN
  names_eval_days <- GerminaR::get_eval_days(fieldbook = fieldbook)
  eval_days <- fieldbook[names_eval_days]
  
  
  if(!is.element("GRS",names(fieldbook))) {fieldbook$GRS <- NA}
  fieldbook <- within(fieldbook,{  
    GRS <- GerminaR::GRS(eval_days = eval_days) 
  })
  
  if(!is.element("GRP",names(fieldbook))) {fieldbook$GRP <- NA}
  fieldbook <- within(fieldbook,{  
    GRP <- GerminaR::GRP(seeds = seeds, eval_days = eval_days) 
  })
  
  if(!is.element("ASG",names(fieldbook))) {fieldbook$ASG <- NA}
  fieldbook <- within(fieldbook,{  
    ASG <- GerminaR::ASG(seeds = seeds,eval_days = eval_days) 
  })
  
  
  if(!is.element("MGT",names(fieldbook))) {fieldbook$MGT <- NA}
  fieldbook <- within(fieldbook,{  
    MGT <- GerminaR::MGT(eval_days = eval_days) 
  })
  
  
  if(!is.element("CVG",names(fieldbook))) {fieldbook$CVG <- NA}
  fieldbook <- within(fieldbook,{  
    CVG <- GerminaR::CVG(eval_days = eval_days) 
  })
  
  
  if(!is.element("MGR",names(fieldbook))) {fieldbook$MGR <- NA}
  fieldbook <- within(fieldbook,{  
   MGR  <- GerminaR::MGR(eval_days = eval_days) 
  })  
  
 
  return(fieldbook)
#   CVG CoVG
# GRP
# GRS, GRU, GSI, MGR, MGT, SDG, VGT
}