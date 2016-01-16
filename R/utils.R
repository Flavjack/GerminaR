#' Evaluation days row names
#' @description This function Calculate the Number of Germinated Seeds
#' @param fieldbook Data frame with the germination evaluation
#' @author Omar Benites
#' @return Names of the evaluation days
#' @export

get_eval_days <- function(fieldbook){
  factors <- c("RPT", "GNT", "SALT", "SDN", "ASG", "CVG", "CoVG", "GRP", "GRS", "GRU", "GSI", "MGR", "MGT", "SDG", "VGT")
  eval_days <- names(fieldbook)
  eval_days <- names(fieldbook)[!is.element(eval_days,set = factors)]
  eval_days
}

#'This function allows sum columns and avoid NA values at the same time
#'
#' @param x vector of measurements
#' @return A vector contains sum of values that avoid NA's
#' @keywords sum, NA's
#' @author Omar Benites 
#' @export
#'
sbsum <- function(x){
  z <- x[!is.na(x)]; 
  ifelse(length(z), sum(z), NA)
} 
