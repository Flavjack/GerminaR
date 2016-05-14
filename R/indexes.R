library(gsheet)
library(agricolae)
library(ggplot2)
library(tidyr)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1G9555qEa1TJ5NDJV4zGAffY-YZTJTE0keDpiANnJpIo/edit#gid=0"

fb<- gsheet2tbl(url) %>% select(RPT:SDN, D00:D25)
#str(fb)


#' Germinated Seed Number
#' @description This function Calculate the Germination percentage
#' @param seeds Total seed sown
#' @param eval_days Germination evaluation days
#' @return Number of seed germianated in the experimen for EU
#' @export
ger_GRS <- function(eval_days){
  ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger
}

#' Germination Seed percentage
#' @description This function Calculate the Germination percentage
#' @param seeds Total seed sown
#' @param eval_days Germination evaluation days
#' @return Percentage of seed germinated
#' @export

ger_GRP <- function(seeds, eval_days){
  ger <-  ger_GRS(eval_days)
  per <- (ger/seeds)*100
  per
}


#' ArcSin of Germination eval_days
#' @description This function Calculate the ArcSin of Germination eval_days for normalization
#' @param seeds Total seed sown
#' @param eval_days Germination evalaluation days
#' @return ArcSin of Germination percentage
#' @export

ger_ASG <- function(seeds, eval_days){
  per <-  ger_GRP(seeds, eval_days)/100
  rst <-  asin(sqrt(per))
  rst
}


#' Mean Germination Time
#' @description This function Calculate the Mean Germination Time
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
#' @description This function Calculate the Coefficient of Velocity of Germination
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
#' @description This function Calculate the Mean Germination Rate
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
#' @description This function Calculate the Germination Uncertainty
#' @param eval_days Germination evalaluation days
#' @return Germination Uncertainty (U)
#' @export

ger_GRU <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  prod <- eval_days/ger * log2(eval_days/ger)
  rst <- apply(prod, 1, sum, na.rm = TRUE) * -1
  rst
}

#' Germination Synchronization Index
#' @description This function Calculate the Germination Synchronization Index
#' @param eval_days Germination evalaluation days
#' @return Germination Synchronization Index (Z)
#' @export

ger_GSI <- function(eval_days){
  ger <-  ger_GRS(eval_days)
  temp <- eval_days*(eval_days - 1)/2
  smp <- apply(temp ,1, sum, na.rm = TRUE)
  rst <- smp / (ger*(ger-1)/2)
  rst
}


#' Variance of Germination Time
#' @description This function Calculate the Variance of Germination Time
#' @param eval_days Germination evalaluation days
#' @return Variance of Germination Time
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


#' Standard deviation of the Germination Time
#' @description This function Calculate the Standard deviation of the Germination Time
#' @param eval_days Germination evalaluation days
#' @return Standard deviation of the Germination Time
#' @export

ger_SDG <- function(eval_days){
  vgt <- ger_VGT(eval_days)
  rst <- sqrt(vgt)
  rst
}


#' Coefficient of Variation of the Mean Germination Time
#' @description This function Calculate the Coefficient of Variation of the Mean Germination Time
#' @param eval_days Germination evalaluation days
#' @return Coefficient of Variation of the Mean Germination Time
#' @export

ger_CVG <- function(eval_days){
  sdg <- ger_SDG(eval_days)
  mgt <- ger_MGT(eval_days)
  rsp <- (sdg/mgt)*100
  rsp
}

