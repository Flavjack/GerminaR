#' Germination Seed Number
#' @description This function Calculate the Number of Germinated Seeds
#' @param eval_days Germination evaluation days
#' @author Flavio Lozano
#' @return Number of seed germinated
#' @export

GRS <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  #ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  ger
}

#' Germination Seed percentage
#' @description This function Calculate the Germination percentage
#' @param seeds Total seed sown
#' @param eval_days Germination evaluation days
#' @author Flavio Lozano
#' @return Percentage of seed germinated
#' @export

GRP <- function(seeds, eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  #ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  per <- (ger/seeds)*100
  
  per
}

#' ArcSin of Germination eval_days
#' @description This function Calculate the ArcSin of Germination eval_days for normalization
#' @param seeds Total seed sown
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return ArcSin of Germination eval_days
#' @export

ASG <- function(seeds, eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  #ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  per <-  (ger/seeds)
  rst <-  asin(sqrt(per))
  rst
}

#' Mean Germination Time
#' @description This function Calculate the Mean Germination Time
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Mean Germination Time
#' @export

MGT <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  #ger <-  apply(cbind(eval_days), 1, sum, na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  #days <- 0:length(eval_days)
  ndays <- ncol(eval_days)-1
  days <- 0:ndays
  temp <- suppressWarnings(t(t(eval_days) * days)) #The transpose is due to multiple each component vector to column in DF
  #rst <- apply(cbind(temp),1, sum, na.rm = T)/ger
  rst <- apply(cbind(temp),1, sbsum)/ger
  rst
}

#' Coefficient of Velocity of Germination
#' @description This function Calculate the Coefficient of Velocity of Germination
#' @param eval_days Germination eval_days
#' @author Flavio Lozanoa
#' @return Coefficient of Velocity of Germination
#' @export

CVG <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  #days <- 0:length(eval_days)
  ndays <- ncol(eval_days)-1
  days <- 0:ndays
  temp <- suppressWarnings(t(t(eval_days) * days))
  #mgt <- sum(days*eval_days, na.rm = T)/ger
  mgt <- apply(cbind(temp),1, sbsum)/ger
  rst <- (1/mgt)*100
  rst
}

#' Mean Germination Rate
#' @description This function Calculate the Mean Germination Rate
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Mean Germination Rate
#' @export

MGR <- function(eval_days){
  
  ger <-  apply(cbind(eval_days), 1, sbsum)
  #days <- 0:length(eval_days)
  ndays <- ncol(eval_days)-1
  days <- 0:ndays
  temp <- suppressWarnings(t(t(eval_days) * days))
  rst <- ger/apply(cbind(temp),1, sbsum)
  rst
}

#' Germination Uncertainty
#' @description This function Calculate the Germination Uncertainty
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Germination Uncertainty (U)
#' @export

GRU <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  #rst <- sum(eval_days/ger * log2(eval_days/ger), na.rm = T) * -1
  prod <- eval_days/ger * log2(eval_days/ger)
  #rst <-  sum(prod, na.rm = T) * -1
  rst <- apply(prod, 1, sbsum) * -1
  rst
  }

#' Germination Synchronization Index
#' @description This function Calculate the Germination Synchronization Index
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Germination Synchronization Index (Z)
#' @export

GSI <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  temp <- eval_days*(eval_days - 1)/2
  #smp <- sum( eval_days*(eval_days - 1)/2 , na.rm = TRUE)
  smp <- apply(temp ,1, sbsum)
  rst <- smp / (ger*(ger-1)/2)
  rst
}

#' Variance of Germination Time
#' @description This function Calculate the Variance of Germination Time
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Variance of Germination Time
#' @export

VGT <- function(eval_days){
  #ger <- sum(eval_days,na.rm = TRUE)
  ger <-  apply(cbind(eval_days), 1, sbsum)
  #days <- 0:length(eval_days)
  ndays <- ncol(eval_days)-1
  days <- 0:ndays
  #mgt <- sum(days*eval_days, na.rm = T)/ger
  mgt <- MGT(eval_days = eval_days)
  #temp <- eval_days * (days-mgt)^2
  temp <- eval_days*(t(t(eval_days)-mgt))^2
  #rsp <- sum(eval_days * (days-mgt)^2, na.rm = TRUE)
  rsp <- apply(temp, 1, sbsum)
  rst <- rsp/(ger-1)
  rst
}

#' Standard deviation of the Germination Time
#' @description This function Calculate the Standard deviation of the Germination Time
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Standard deviation of the Germination Time
#' @export

SDG <- function(eval_days){
  ger <- sum(eval_days,na.rm = TRUE)
  days <- 0:length(eval_days)
  mgt <- sum(days*eval_days, na.rm = T)/ger
  rsp <- sum(eval_days * (days-mgt)^2, na.rm = TRUE)
  vgt <- rsp/(ger-1)
  rst <- sqrt(vgt)
  rst
}

#' Coefficient of Variation of the Mean Germination Time
#' @description This function Calculate the Coefficient of Variation of the Mean Germination Time
#' @param eval_days Germination eval_days
#' @author Flavio Lozano
#' @return Coefficient of Variation of the Mean Germination Time
#' @export

CoVG <- function(eval_days){
  ger <- sum(eval_days,na.rm = TRUE)
  days <- 0:length(eval_days)
  mgt <- sum(days*eval_days, na.rm = T)/ger
  rsp <- sum(eval_days * (days-mgt)^2, na.rm = TRUE)
  vgt <- rsp/(ger-1)
  sdg <- sqrt(vgt)
  rst <- (sdg/mgt)*100
  rst
}




