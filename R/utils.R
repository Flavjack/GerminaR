#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' Repeated Rows in a data matrix
#' 
#' @description This function made a data table with the evaluation days of germination
#' @param Rseq Row sequance for the data matrix
#' @param Nrow Number of rows for the data matrix
#' @return Data Matrix with day of the germination
#' @export

rep_row<-function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
}


#' Select colum according the initial letters of the words
#' 
#' @description Select colum according the initial letters of the words
#' @param vars vars
#' @param match match 
#' @param ignore.case case
#' @return Matrix with the select colum
#' @author Hadley Wickham
#' @details https://github.com/hadley/dplyr/blob/50309db8f04cbcc87e4568a4bfa1f0c718e824c9/R/select-utils.R
#' @importFrom assertthat is.string

starts_with <- function(vars, match, ignore.case = TRUE) {
  stopifnot(is.string(match), !is.na(match), nchar(match) > 0)
  
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)
  
  if (ignore.case) vars <- tolower(vars)
  which(substr(vars, 1, n) == match)
}


#' Select Evaluation Days
#' 
#' @description Give matrix with the evaluation days
#' @param data Data with germination values
#' @param evalName Prefix of the evalaution variable
#' @return Evalution colums
#' @export
#' @examples 
#' \dontrun{ 
#' library(GerminaR)
#' dt <- prosopis
#' dm <- evalDays(evalName = "D", data = dt)
#' dm
#' }

evalDays <- function(evalName, data){
  
    evd <- dplyr::select(data, starts_with(colnames(data), evalName))
    evd

}


#' Select Factors of germination matrix
#' 
#' @description Give matrix with the factor
#' @param evalName Prefix of the evalaution variable
#' @param data Data with germination values
#' @return Factor columsl
#' @export
#' @examples 
#' \dontrun{ 
#' library(GerminaR)
#' dt <- prosopis
#' dm <- evalFactor(evalName = "D", data = dt)
#' dm
#' }

evalFactor <- function(evalName, data){
  
  evf <- dplyr::select(data, -starts_with(colnames(data), evalName))
  
  #evf[,colnames(evf)] <- lapply(evf[,colnames(evf)] , as.character)
  
  evf
  
}


#' Mean Comparison Table Summary
#' 
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs. 
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @importFrom dplyr mutate select rename group_by_ summarise full_join
#' @importFrom tidyr separate
#' @export

dtsm <- function(meanComp){
  
  #to avoid no bisible global variable function
  std <- r <- trt <- means <- Min <- Max <- ste <- M <- NULL
  
  #fct <- as.character(mc$parameters$name.t)
  fct <- as.character(meanComp$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))
  
  #dtmn <- mc$means #flavio
  dtmn <- meanComp$means #omar
  #dtgr <- mc$groups #flavio
  dtgr <- meanComp$groups #omar
  
  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))
  
  dta <- dtmn %>% 
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn))) 
               
  sm <- dplyr::full_join(dta[2:7], dtgr, by = "trt") %>% 
    dplyr::select(trt, means, Min, Max, r, std, ste, M) %>% 
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>% 
    dplyr::rename(mean = means, min = Min, max = Max, sg = M)
  

}



#' Multiple comparison test
#'
#' @description Function analisis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparision analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return Table with complete data for graphics
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @export


ger_testcomp <- function( aov, comp, type = "snk", sig = 0.05){
  
  if( type == "snk"){
    
    mc <- agricolae::SNK.test(y = aov, trt = comp, alpha = sig)
    
  } else if (type == "tukey"){
    
    mc <- agricolae::HSD.test(y = aov, trt = comp, alpha = sig)
    
  } else if (type == "duncan"){
    
    mc <- agricolae::duncan.test(y = aov, trt = comp, alpha = sig)
    
  }
  
  GerminaR::dtsm(mc)
  
}


#' Regresion line equation
#'
#' @description Construc the regression line equation
#' @param data dataframe with the information
#' @param y variable in the y axis
#' @param x variable in the x axis
#' @return regression equation
#' @importFrom stats as.formula coef
#' @export

ger_leq <- function(x, y, data){
  
  fml <- as.formula(paste( x , y, sep = " ~ "))
  mdl <- lm(fml, data)
  
  eq <- as.character(as.expression(
    substitute(italic(y) == a + (b) * italic(x) * "," ~~ italic(R)^2 ~ "=" ~ r2,
               list(a = format(coef(mdl)[1], digits=2), b = format(coef(mdl)[2], digits=2),
                    r2 = format(summary(mdl)$r.squared, digits=3) ))))
  
  eq
  
  # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
  #                  list(a = format(coef(mdl)[1], digits = 2),
  #                       b = format(coef(mdl)[2], digits = 2),
  #                       r2 = format(summary(mdl)$r.squared, digits = 3)))
  # as.character(as.expression(eq))
  
  
}


#' Import google spreadsheet or xlsx file
#'
#' @description function to import information from google spreadsheet or xlsx file.
#' @param dir local file directory for xlsx document or url from google spreadsheet
#' @param sheet if is a xlsx file, you can choose the sheet number
#' @return data frame
#' @importFrom gsheet gsheet2tbl
#' @importFrom readxl read_excel
#' @importFrom dplyr  '%>%'
#' @export

ger_getdata <- function(dir, sheet = 1) {
  
  
  if (file.exists(dir) == TRUE) {
    
    readxl::read_excel(path = dir, sheet = sheet) %>% as.data.frame()
    
  } else{
    
    gsheet::gsheet2tbl(url = dir) %>% as.data.frame()
    
  }
  
  
}


#' Descriptive Statistics for a model
#'
#' @description Function to summary descriptive statistics from a model
#' @param modelo an object containing the results returned by a model fitting function
#' @param data data set used for the model
#' @return data frame
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%' select_
#' @importFrom stats anova
#' @export

stat_sm <- function(modelo, data){
  
  avt <- anova(modelo)
  
  varn <- colnames(modelo[["model"]][1])
  
  
  smr <- data %>% 
    select_( varn )
  
  MSerror <- SDev <- Mean <- NULL
  
  smd <- smr %>%
    dplyr::summarise(
      MSerror  =  avt["Residuals", 3],
      Mean = mean(smr[[varn]], na.rm=TRUE),
      SDev = sd(smr[[varn]], na.rm=TRUE),
      Min = min(smr[[varn]], na.rm=TRUE),
      Max = max(smr[[varn]], na.rm=TRUE),
      Num = sum(!is.na(smr[[varn]])),
      CV = sqrt(MSerror) * 100/Mean
      
    )
  
  smd
  
}

