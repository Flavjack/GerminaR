
#' Germinated Seed Number
#' @description This function calculate the number of seed germinated. 
#' @param eval_days Germination evaluation days
#' @return Number of seed germianated 
#' @export
ger_GRS <- function(eval_days){
  ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger
}

#' Germination Seed Percentage
#' @description This function calculate the germination percentage related at total seed sown.
#' @param seeds Total seed sown
#' @param eval_days Germination evaluation days
#' @return Percentage of seed germinated.
#' @export

ger_GRP <- function(seeds, eval_days){
  ger <-  ger_GRS(eval_days)
  per <- (ger/seeds)*100
  per
}


#' ArcSin of Germination Percentage
#' @description This function calculate the arcsin of germination percentage for normalization
#' @param seeds Total seed sown
#' @param eval_days Germination evalaluation days
#' @return ArcSin of Germination
#' @export

ger_ASG <- function(seeds, eval_days){
  per <-  ger_GRP(seeds, eval_days)/100
  rst <-  asin(sqrt(per))
  rst
}


#' Mean Germination Time
#' @description This function calculate the mean germination time of germination according at the time lapse of the evaluations
#' @param eval_days Germination evalaluation days
#' @return Mean Germination Time
#' @export

ger_MGT <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  days <- 0:(ncol(eval_days)-1)
  temp <- t(t(eval_days) * days)
  rst <- apply(cbind(temp),1, sum, na.rm = T)/ger
  rst
}


#' Coefficient of Velocity of Germination
#' @description This function calculate the coefficient of velocity of germination according at the time lapse of the evaluations
#' @param eval_days Germination evalaluation days
#' @return Coefficient of Velocity of Germination
#' @export

ger_CVL <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  days <- 0:(ncol(eval_days)-1)
  temp <- t(t(eval_days) * days)
  mgt <- apply(cbind(temp),1, sum, na.rm = TRUE)/ger
  rst <- (1/mgt)*100
  rst
}


#' Mean Germination Rate
#' @description This function calculate the mean germination rate of the germination
#' @param eval_days Germination evalaluation days
#' @return Mean Germination Rate
#' @export

ger_MGR <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  days <- 0:(ncol(eval_days)-1)
  temp <- t(t(eval_days) * days)
  rst <- ger/apply(cbind(temp),1, sum, na.rm = TRUE)
  rst
}


#' Germination Uncertainty
#' @description This function calculate the germination uncertainty of the germination process.
#' @param eval_days Germination evalaluation days
#' @return Germination Uncertainty
#' @export

ger_GRU <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  prod <- eval_days/ger * log2(eval_days/ger)
  rst <- apply(prod, 1, sum, na.rm = TRUE) * -1
  rst
}

#' Germination Synchronization Index
#' @description This function calculate the germination synchronization of the germination process
#' @param eval_days Germination evalaluation days
#' @return Germination Synchrony
#' @export

ger_GSI <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  temp <- eval_days*(eval_days - 1)/2
  smp <- apply(temp ,1, sum, na.rm = TRUE)
  rst <- smp / (ger*(ger-1)/2)
  rst
}


#' Variance of the Mean Germination Time
#' @description This function calculate the variance of the mean germination time.
#' @param eval_days Germination evalaluation days
#' @return Variance of Germination
#' @export

ger_VGT <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  mgt <- ger_MGT(eval_days)
  days <- 0:(ncol(eval_days)-1)
  daym <- rep.row(days,nrow(eval_days))
  temp <- eval_days * (daym-mgt)^2
  rsp <- apply(temp, 1, sum, na.rm = TRUE)
  rst <- rsp/(ger-1)
  rst
}


#' Standard deviation of the Mean Germination Time
#' @description This function calculate the standard desviation of the mean germination time
#' @param eval_days Germination evalaluation days
#' @return Standard desviation of germination
#' @export

ger_SDG <- function(eval_days){
  vgt <- ger_VGT(eval_days)
  rst <- sqrt(vgt)
  rst
}


#' Coefficient of Variance of the Mean Germination Time
#' @description This function calculate the coefficient of variation of the mean germination time
#' @param eval_days Germination evalaluation days
#' @return Coefficient of Variance of germination
#' @export

ger_CVG <- function(eval_days){
  sdg <- ger_SDG(eval_days)
  mgt <- ger_MGT(eval_days)
  rsp <- (sdg/mgt)*100
  rsp
}

