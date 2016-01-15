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

GRP <- function(seeds, values){
  sum <- sum(values,na.rm = TRUE)
  per <- (sum/seeds)*100
  per
}

#' ArcSin of Germination Values
#' @description This function Calculate the ArcSin of Germination Values for normalization
#' @param seeds Total seed sown
#' @param values Germination values
#' @author Flavio Lozano
#' @return ArcSin of Germination Values
#' @export

ASG <- function(seeds, values){
  sum <- sum(values,na.rm = TRUE)
  per <- (sum/seeds)
  rst <- asin(sqrt(per))
  rst
}

#' Mean Germination Time
#' @description This function Calculate the ArcSin of Germination Values for normalization
#' @param values Germination values
#' @author Flavio Lozano
#' @return ArcSin of Germination Values
#' @export

MGT <- function(values){
  sum <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  rst <- sum(days*values, na.rm = T)/sum
  rst
}


#' Mean Germination Rate
#' @description This function Calculate the Mean Germination Rate
#' @param values Germination values
#' @author Flavio Lozano
#' @return Mean Germination Rate
#' @export

MGR <- function(values){
  sum <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  rst <- sum/sum(days*values, na.rm = T)
  rst
}



