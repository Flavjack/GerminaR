
#' Germinated Seed Number
#' @description This function calculate the number of seed germinated. 
#' @param evalDays Germination evaluation days
#' @return Number of seed germianated 
#' @export
ger_GRS <- function(evalDays){
  ger <-  apply(cbind(evalDays), 1, sum, na.rm = TRUE)
  ger
}

#' Germination Seed Percentage
#' @description This function calculate the germination percentage related at total seed sown.
#' @param SeedN Name of the colon with the seed numbers
#' @param evalDays Germination evaluation days
#' @return Percentage of seed germinated.
#' @export

ger_GRP <- function(SeedN, evalDays){
  ger <-  ger_GRS(evalDays)
  per <- (ger/SeedN)*100
  per
}


#' ArcSin of Germination Percentage
#' @description This function calculate the arcsin of germination percentage for normalization
#' @param SeedN Name of the colon with the seed numbers
#' @param evalDays Germination evalaluation days
#' @return ArcSin of Germination
#' @export

ger_ASG <- function(SeedN, evalDays){
  per <-  ger_GRP(SeedN, evalDays)/100
  rst <-  asin(sqrt(per))
  rst
}


#' Mean Germination Time
#' @description This function calculate the mean germination time of germination according at the time lapse of the evaluations
#' @param evalDays Germination evalaluation days
#' @return Mean Germination Time
#' @export

ger_MGT <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  days <- 0:(ncol(evalDays)-1)
  temp <- t(t(evalDays) * days)
  rst <- apply(cbind(temp),1, sum, na.rm = T)/ger
  rst
}


#' Coefficient of Velocity of Germination
#' @description This function calculate the coefficient of velocity of germination according at the time lapse of the evaluations
#' @param evalDays Germination evalaluation days
#' @return Coefficient of Velocity of Germination
#' @export

ger_CVL <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  days <- 0:(ncol(evalDays)-1)
  temp <- t(t(evalDays) * days)
  mgt <- apply(cbind(temp),1, sum, na.rm = TRUE)/ger
  rst <- (1/mgt)*100
  rst
}


#' Mean Germination Rate
#' @description This function calculate the mean germination rate of the germination
#' @param evalDays Germination evalaluation days
#' @return Mean Germination Rate
#' @export

ger_MGR <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  days <- 0:(ncol(evalDays)-1)
  temp <- t(t(evalDays) * days)
  rst <- ger/apply(cbind(temp),1, sum, na.rm = TRUE)
  rst
}


#' Germination Uncertainty
#' @description This function calculate the germination uncertainty of the germination process.
#' @param evalDays Germination evalaluation days
#' @return Germination Uncertainty
#' @export

ger_GRU <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  prod <- evalDays/ger * log2(evalDays/ger)
  rst <- apply(prod, 1, sum, na.rm = TRUE) * -1
  rst
}

#' Germination Synchronization Index
#' @description This function calculate the germination synchronization of the germination process
#' @param evalDays Germination evalaluation days
#' @return Germination Synchrony
#' @export

ger_GSI <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  temp <- evalDays*(evalDays - 1)/2
  smp <- apply(temp ,1, sum, na.rm = TRUE)
  rst <- smp / (ger*(ger-1)/2)
  rst
}


#' Variance of the Mean Germination Time
#' @description This function calculate the variance of the mean germination time.
#' @param evalDays Germination evalaluation days
#' @return Variance of Germination
#' @export

ger_VGT <- function(evalDays){
  ger <-  ger_GRS(evalDays)
  mgt <- ger_MGT(evalDays)
  days <- 0:(ncol(evalDays)-1)
  daym <- rep.row(days,nrow(evalDays))
  temp <- evalDays * (daym-mgt)^2
  rsp <- apply(temp, 1, sum, na.rm = TRUE)
  rst <- rsp/(ger-1)
  rst
}


#' Standard deviation of the Mean Germination Time
#' @description This function calculate the standard desviation of the mean germination time
#' @param evalDays Germination evalaluation days
#' @return Standard desviation of germination
#' @export

ger_SDG <- function(evalDays){
  vgt <- ger_VGT(evalDays)
  rst <- sqrt(vgt)
  rst
}


#' Coefficient of Variance of the Mean Germination Time
#' @description This function calculate the coefficient of variation of the mean germination time
#' @param evalDays Germination evalaluation days
#' @return Coefficient of Variance of germination
#' @export

ger_CVG <- function(evalDays){
  sdg <- ger_SDG(evalDays)
  mgt <- ger_MGT(evalDays)
  rsp <- (sdg/mgt)*100
  rsp
}

