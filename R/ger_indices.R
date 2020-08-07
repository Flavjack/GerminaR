#' Germinated Seed Number
#' 
#' @export
#' @description
#' This function calculates the number of seed germinated. 
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return Number of seed germinated
#' @examples 
#' 
#' library(GerminaR)
#' dt <- prosopis
#' grs <- ger_GRS(evalName = "D", data = dt)
#' grs

ger_GRS <- function(evalName, data){
  
  evd <-  data %>% 
    select(starts_with({{evalName}})) 
  
  ger <- apply(cbind(evd), 1, sum, na.rm = TRUE)
  ger
  
}

#' Germination Seed Percentage
#' 
#' @description 
#' This function calculates the germination percentage related at total seed sown for experimental unit.
#' @details 
#' According GOUVEA LABOURIAU (1983), the germinability of a sample of is the percentage of 
#' seeds in which the seed germination process comes to an end, in experimental conditions by 
#' the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo.
#' @references
#' LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.
#' @param SeedN Name of the column with the number of seeds sown.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the percentage of seed germinated.
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dt <- prosopis
#' grp <- ger_GRP(SeedN = "seeds",evalName = "D", data = dt)
#' grp

ger_GRP <- function(SeedN, evalName, data){
  
  sdn <- data[, SeedN]
  sdn <- as.numeric(sdn)
  grs <- ger_GRS(evalName, data)
  tmp <- grs/sdn * 100
  tmp
  
}

#' ArcSin of Germination Percentage
#' 
#' @description 
#' This function calculates the arcsin of germination percentage for normalization.
#' @param SeedN Name of the column with the number of seeds sown.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the ArcSin of Germination values
#' @export
#' @examples
#' library(GerminaR)
#' dfr <- prosopis
#' gas <- ger_ASG(SeedN = "seeds", evalName = "D", data = dfr)
#' gas

ger_ASG <- function(SeedN, evalName, data){
  
  grp <-  ger_GRP(SeedN, evalName, data)
  tmp <-  grp/100
  rst <-  asin(sqrt(tmp)) 
  rst
}

#' Mean Germination Time
#' 
#' @description 
#' This function calculates the mean germination time of germination according at the time lapse of the evaluations.
#' @details 
#' It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. 
#' The number of germinated seeds at the intervals established for the collection of data is used as weight. 
#' It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962).
#' @references 
#' CZABATOR, F. J. Germination value: an index combining speed and completeness of pine seed germination. 
#' Forest Science, v. 8, n. 4, p. 386-396, 1962.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the values of Mean Germination Time.
#' @importFrom dplyr select starts_with
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dfr <- prosopis
#' mgt <- ger_MGT(evalName = "D", data = dfr)
#' mgt

ger_MGT <- function(evalName, data){
  
  ger <- ger_GRS(evalName, data)
  evd <- data %>% select(starts_with(evalName))
  evcn <- colnames(evd)
  day <- as.numeric(gsub("\\D", "", evcn))
  tmp <- t(t(evd) * day)
  rst <- apply(cbind(tmp),1, sum, na.rm = T)
  rst/ger
  
}

#' Mean Germination Rate
#' 
#' @description 
#' This function calculates the mean germination rate of the germination.
#' @details 
#' The average speed of germination is defined as the reciprocal of the average time germination (RANAL; SANTANA, 2006).
#' @references 
#' RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process? 
#' Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the values of Mean Germination Rate
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' mgr <- ger_MGR(evalName = "D", data = dfr)
#' mgr

ger_MGR <- function(evalName, data){
  
  mgt <- ger_MGT(evalName, data)
  rst <- 1/mgt
  rst
  
}

#' Germination Speed
#' 
#' @description This function calculates the Germination Speed according at the time lapse of the evaluations.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return  It returns an vector with the Germination Speed
#' @importFrom dplyr select starts_with
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' gsp <- ger_GSP(evalName = "D", data = dfr)
#' gsp

ger_GSP <- function(evalName, data){
  
  grs <- ger_GRS(evalName, data)
  evd <- data %>% select(starts_with(evalName))
  evcn <- colnames(evd)
  day <- as.numeric(gsub("\\D", "", evcn))
  tmp <- t(t(evd) * day)
  cal <- apply(cbind(tmp),1, sum, na.rm = T)
  rst <- grs/cal*100
  rst
  
}

#' Germination Uncertainty
#' 
#' @description 
#' This function calculates the germination uncertainty in the germination process.
#' @details 
#' The uncertainty index \code{u} is an adaptation of Shannon index measures the degree of uncertainty in predicting the informational 
#' entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). 
#' Low values of \code{u} indicate frequencies with short peaks, i.e. the more concentrated the germination in time. 
#' Just a germinated seed changes the value of \code{u}. This means that \code{u} measures the degree of germination scattering.
#' @references 
#' GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington.
#' LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return  It returns an vector with the values of Germination Uncertainty.
#' @importFrom dplyr select starts_with
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' unc <- ger_UNC(evalName = "D", data = dfr)
#' unc

ger_UNC <- function(evalName, data){
  
  grs <- ger_GRS(evalName, data)
  evd <- data %>% select(starts_with(evalName))
  tmp <- evd/grs * log2(evd/grs)
  rst <- apply(tmp, 1, sum, na.rm = TRUE)
  abs(rst)
  
}

#' Germination Synchronization Index
#' 
#' @description 
#' This function calculates the germination synchronization of the germination process.
#' @details
#' The Synchory Index \code{Z} has been proposed to assess the degree of overlap between flowering individuals in a population. 
#' By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. 
#' \code{Z} = 1 when germination of all the seeds occurs at the same time and \code{Z} = 0 when at least two seeds can germinate one each time. 
#' \code{Z} produces a number if and only if there are two seeds finishing the seed germination process at the same time. 
#' Thus, the value of \code{Z} assessments is the grade of overlap between seed germination.
#' @references 
#' RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process? 
#' Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return  It returns an vector with the values of Germination synchrony
#' @importFrom dplyr select starts_with
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' syn <- ger_SYN(evalName = "D", data = dfr)
#' syn

ger_SYN <- function(evalName, data){
  
  grs <- ger_GRS(evalName, data)
  evd <- data %>% select(starts_with(evalName))
  tmp <- evd*(evd - 1)/2
  cal <- apply(tmp ,1, sum, na.rm = TRUE)
  rst <- cal/(grs*(grs-1)/2)
  rst
  
}

#' Variance of the Mean Germination Time
#' 
#' @description 
#' This function calculates the variance of the mean germination time.
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the values of Variance of Germination
#' @importFrom dplyr select starts_with
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' vgt <- ger_VGT(evalName = "D", data = dfr)
#' vgt

ger_VGT <- function(evalName, data){
  
  grs <- ger_GRS(evalName, data)
  mgt <- ger_MGT(evalName, data)
  evd <- data %>% select(starts_with(evalName))
  day <- 0:(ncol(evd)-1)
  dym <- rep_row(day,nrow(evd)) # Matrix for product of matrix
  tmp <- evd * (dym-mgt)^2
  cal <- apply(tmp, 1, sum, na.rm = TRUE)
  rst <- cal/(grs-1)
  rst
  
}

#' Standard deviation of the Mean Germination Time
#' 
#' @description 
#' This function calculates the standard deviation of the mean germination time
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the values of Standard deviation of germination
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' sdg <- ger_SDG(evalName = "D", data = dfr)
#' sdg

ger_SDG <- function(evalName, data){
  
  vgt <- ger_VGT(evalName, data)
  rst <- sqrt(vgt)
  rst
  
}

#' Coefficient of Variance of the Mean Germination Time
#' 
#' @description 
#' This function calculates the coefficient of variation of the mean germination time
#' @param evalName Prefix of the names of the periods of evaluation.
#' @param data The name of the data frame containing the data.
#' @return It returns an vector with the values of Coefficient of Variance of germination
#' @export
#' @examples 
#' library(GerminaR)
#' dfr <- prosopis
#' cvg <- ger_CVG(evalName = "D", data = dfr)
#' cvg

ger_CVG <- function(evalName, data){
  
  sdg <- ger_SDG(evalName,  data)
  mgt <- ger_MGT(evalName,  data)
  rsp <- (sdg/mgt)*100
  rsp
  
}
