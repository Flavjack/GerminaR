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

rep_row <- function(Rseq,Nrow){
  matrix(rep(Rseq,each=Nrow),nrow=Nrow)
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
  
    evd <- data %>% 
      dplyr::select(starts_with(evalName))
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
  
  evf <- data %>% 
    dplyr::select(-starts_with(evalName))
  
  evf
  
}


#' Mean Comparison Table Summary
#' 
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs. 
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @importFrom dplyr mutate select rename_ group_by_ summarise full_join rename
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr separate
#' @export

dtsm <- function(meanComp){
  
  #to avoid no bisible global variable function
  std <- r <- trt <- mean <- Min <- Max <- ste <- groups <- NULL
  
  fct <- as.character(meanComp$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))
  
  dtmn <- meanComp$means %>% rename_(mean = names(meanComp$means[1]))
 
  dtgr <- meanComp$groups %>% rownames_to_column(var = "trt")
  
  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))
  
  dta <- dtmn %>% 
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn))) 
               
  sm <- dplyr::full_join(dta, dtgr, by = "trt") %>% 
    dplyr::select(trt, mean, Min, Max, r, std, ste, groups) %>% 
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>% 
    dplyr::rename(min = Min, max = Max, sg = groups)
  
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
  
  fml <- as.formula(paste( y , x, sep = " ~ "))
  mdl <- lm(fml, data)
  
  eq <- substitute(italic(y) == a + b*italic(x)*',' ~~ italic(R)^2 ~ "=" ~ r2,
               list(a = format(unname(coef(mdl)[1]), digits=3),
                    b = format(unname(coef(mdl)[2]), digits=3),
                    r2 = format(summary(mdl)$r.squared, digits=3)))
  
  as.character(as.expression(eq))
  
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

#' Osmotic potential calculator
#'
#' @description Function to calculate the grams of salt or PEG-6000 needed for determinated osmotic potential 
#' @param type Salt or PEG-6000 c("salt", "peg6000"). Default: "salt".
#' @param vol volume (liters)
#' @param pres Pressure (Mpa) in negative values. 1 bar = 0.1 Mpa
#' @param temp Temperature (centigrade)
#' @param mw Molecular weight
#' @param ki Salt dissociation constant (NaCl = 1.8)
#' @return Numeric value (grams)
#' @export

osmp <- function(type = "salt", vol, pres, temp, mw, ki){
  
  if( type == "salt"){
    
    r1 <- (-0.00820574587 * (temp + 273) * ki)
    
    r2 <- pres/r1
    
    r3 <- r2 * 1000
    
    r4 <- (mw * r3 * vol * 1000)/10^6
    
    round(r4, 5)
    
  } else if (type == "peg6000"){
    
    a <- (8.39*10^(-7)*temp - 1.18*10^(-4))
    
    b <- (2.67*10^(-4)*temp - 1.18*10^(-2))
    
    c <- -pres*10
    
    dlt <- b^(2)-(4*a*c)
    
    x <- (-b - sqrt(dlt))/(2*a)
    
    round(x*vol, 5)  

  }
  
}


#' HTML tables for markdown documents
#'
#' @description Export tables with download, pasta and copy buttons
#' @param data dataset
#' @param digits digits number in the table exported
#' @param title Title for the table
#' @param rnames row names
#' @param buttons "excel", "copy" or "none". Default c("excel", "copy")
#' @return table in markdown format for html documents
#' @importFrom dplyr mutate_if
#' @importFrom DT datatable
#' @export

web_table <- function(data, title = NULL, digits = 3, rnames = FALSE, buttons = NULL){
  
  library(DT)
  
  if (is.null(buttons)){
    
    data %>% 
      mutate_if(is.numeric, ~round(., digits)) %>% 
      datatable(extensions = c('Buttons', 'Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = c("excel", "copy"),
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  title)
    
  } else if (buttons == "none"){
    
    data %>% 
      mutate_if(is.numeric, ~round(., digits)) %>% 
      datatable(extensions = c('Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  title)
    
  } else {
    
    data %>% 
      mutate_if(is.numeric, ~round(., digits)) %>% 
      datatable(extensions = c('Buttons','Scroller'),
                rownames = rnames,
                options = list(dom = 'Bt',
                               buttons = buttons,
                               autoWidth = TRUE, scroller = TRUE,
                               scrollY = "50vh", scrollX = TRUE),
                caption =  title)
    
  }
  
}
