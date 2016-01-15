#' Germination Seed Number
#' @description This function Calculate the Number of Germinated Seeds
#' @param values Germination values
#' @author Flavio Lozano
#' @return Number of seed germinated
#' @export

GRS <- function(values){
  ger <- sum(values,na.rm = TRUE)
  ger
}

#' Germination Seed percentage
#' @description This function Calculate the Germination percentage
#' @param seeds Total seed sown
#' @param values Germination values
#' @author Flavio Lozano
#' @return Percentage of seed germinated
#' @export

GRP <- function(seeds, values){
  ger <- sum(values,na.rm = TRUE)
  per <- (ger/seeds)*100
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
  ger <- sum(values,na.rm = TRUE)
  per <- (ger/seeds)
  rst <- asin(sqrt(per))
  rst
}

#' Mean Germination Time
#' @description This function Calculate the Mean Germination Time
#' @param values Germination values
#' @author Flavio Lozano
#' @return Mean Germination Time
#' @export

MGT <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  rst <- sum(days*values, na.rm = T)/ger
  rst
}

#' Coefficient of Velocity of Germination
#' @description This function Calculate the Coefficient of Velocity of Germination
#' @param values Germination values
#' @author Flavio Lozano
#' @return Coefficient of Velocity of Germination
#' @export

cvg <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  mgt <- sum(days*values, na.rm = T)/ger
  rst <- (1/mgt)*100
  rst
}

#' Mean Germination Rate
#' @description This function Calculate the Mean Germination Rate
#' @param values Germination values
#' @author Flavio Lozano
#' @return Mean Germination Rate
#' @export

MGR <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  rst <- ger/sum(days*values, na.rm = T)
  rst
}


#' Germination Uncertainty
#' @description This function Calculate the Germination Uncertainty
#' @param values Germination values
#' @author Flavio Lozano
#' @return Germination Uncertainty (U)
#' @export

GRU <- function(values){
  ger <- sum(values,na.rm = TRUE)
  rst <- sum(values/ger * log2(values/ger), na.rm = T) * -1
  rst
  }

#' Germination Synchronization Index
#' @description This function Calculate the Germination Synchronization Index
#' @param values Germination values
#' @author Flavio Lozano
#' @return Germination Synchronization Index (Z)
#' @export

GSI <- function(values){
  ger <- sum(values,na.rm = TRUE)
  smp <- sum( values*(values - 1)/2 , na.rm = TRUE)
  rst <- smp / (ger*(ger-1)/2)
  rst
  }

#' Variance of Germination Time
#' @description This function Calculate the Variance of Germination Time
#' @param values Germination values
#' @author Flavio Lozano
#' @return Variance of Germination Time
#' @export

VGT <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  mgt <- sum(days*values, na.rm = T)/ger
  rsp <- sum(values * (days-mgt)^2, na.rm = TRUE)
  rst <- rsp/(ger-1)
  rst
}


#' Standard deviation of the Germination Time
#' @description This function Calculate the Standard deviation of the Germination Time
#' @param values Germination values
#' @author Flavio Lozano
#' @return Standard deviation of the Germination Time
#' @export

SDG <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  mgt <- sum(days*values, na.rm = T)/ger
  rsp <- sum(values * (days-mgt)^2, na.rm = TRUE)
  vgt <- rsp/(ger-1)
  rst <- sqrt(vgt)
  rst
}


#' Coefficient of Variation of the Mean Germination Time
#' @description This function Calculate the Coefficient of Variation of the Mean Germination Time
#' @param values Germination values
#' @author Flavio Lozano
#' @return Coefficient of Variation of the Mean Germination Time
#' @export

CoVG <- function(values){
  ger <- sum(values,na.rm = TRUE)
  days <- 0:length(values)
  mgt <- sum(days*values, na.rm = T)/ger
  rsp <- sum(values * (days-mgt)^2, na.rm = TRUE)
  vgt <- rsp/(ger-1)
  sdg <- sqrt(vgt)
  rst <- (sdg/mgt)*100
  rst
}


