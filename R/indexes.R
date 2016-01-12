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


#' Germination Seed percentage
#' @description This function Calculate the Germination percentage
#' @param seeds Total seed sown
#' @param values Germination values
#' @author Flavio Lozano
#' @return Percentage of seed germinated
#' @export

GSP <- function(seeds, values){
  sum <- sum(values,na.rm = TRUE)
  per <- (sum/seeds)*100
  per
}


