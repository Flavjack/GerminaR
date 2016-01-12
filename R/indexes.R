#' Germination Seed Number
#' @description This function Calculate the Number of Germinated Seeds
#' @param values Germination values
#' @author Flavio Lozano
#' @return Number of seed germinated
#' @export

GRS <- function(values){
  sum <- sum(values,na.rm = TRUE)
  sum
}

