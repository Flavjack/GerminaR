#' Repeated Rows in a data matrix
#' 
#' @description This function made a data table with the evaluation days of germination
#' @param Rseq Row sequance for the data matrix
#' @param Nrow Number of rows for the data matrix
#' @return Data Matrix with day of the germination
#' @export

rep.row<-function(Rseq,Nrow){
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
#' @importFrom dplyr select
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dt <- GerminaR
#' dm <- evalDays(evalName = "Ev", data = dt)
#' dm

evalDays <- function(evalName, data){
  
    evd <- dplyr::select(data, starts_with(colnames(data), evalName))
    evd

}


#' Select Factors of germination matrix
#' 
#' @description Give matrix with the factor
#' @param evalName Prefix of the evalaution variable
#' @param data Data with germination values
#' @return Factor colums
#' @importFrom dplyr select
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' dt <- GerminaR
#' dm <- evalFactor(evalName = "Ev", data = dt)
#' dm

evalFactor <- function(evalName, data){
  
  evf <- dplyr::select(data, -starts_with(colnames(data), evalName))
  
  evf[,colnames(evf)] <- lapply(evf[,colnames(evf)] , as.character)
  
  evf
  
}


#' Mean Comparison Table Summary
#' 
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs. 
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @export
#' @examples 
#' 
#' library(GerminaR)
#' library(agricolae)
#' library(ggplot2)
#' 
#' dt <- GerminaR
#' sm <- ger_summary(SeedN = "NSeeds", evalName = "Ev", data = dt)
#' 
#' av <- aov(MGT ~ Genotype*Salt, sm)
#' summary(av)
#' mc <- SNK.test(av, c("Genotype", "Salt"))
#' 
#' gr <- dtsm(mc)
#' 
#' ggplot(gr, aes(Genotype , mean, fill= factor(Salt, levels = c(0, 50, 75, 100, 150))))+
#'   geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
#'   geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2, position=position_dodge(.9)) +
#'   geom_text(aes(label= sg), colour="black" , vjust=-.5,  hjust= -.5 , angle = 90, size=4 , position=position_dodge(.9))+
#'   scale_y_continuous("Mean Germination Time", limits = c(0, 15), breaks= 0:15*3) +
#'   scale_fill_hue("Salt (mM)")+
#'   theme_bw()

dtsm <- function(meanComp){
  
  fct <- as.character(mc$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))
  
  dtmn <- mc$means
  dtgr <- mc$groups
  dtgr$trt <- gsub("\\s", "",as.character(dtgr$trt))
  
  dta <- dtmn %>% 
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn))) 
               
  sm <- dplyr::full_join(dta[2:7], dtgr, by = "trt") %>% 
    dplyr::select(trt, means, Min, Max, r, std, ste, M) %>% 
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>% 
    dplyr::rename(mean = means, min = Min, max = Max, sg = M)
  
}



#' Plot line or bar graphic
#' 
#' @description Function use the dtsm funtion for plot the results 
#' @param data Output dtsm fuction
#' @param type Type of graphic. "bar" or "line" 
#' @param x Axis x variable
#' @param y Axis t variable
#' @param z variable for color and shape
#' @param ylab Title for the axis y 
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd Possition of the legend. Default c(0.93,0.77). If "none" will not have legend
#' @param lmt Limit of the axis y for the graphic. ie c(0,100)
#' @param brk Brake in the axis y for the graphic. ie 0:100*20
#' @param sig Significance of the result (letters)
#' @return Line o bar plot
#' @export

plot <- function(data, type= c("bar", "line"), x, y, z, lmt, brk, lgd = c(0.93,0.77), ylab = "", xlab = "", lgl = "", sig = sg){
  
  type <- match.arg(type)
  
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  z <- deparse(substitute(z))
  sig <- deparse(substitute(sig))  
  
  if(type == "bar"){
    
    bp <- ggplot(data, aes_string(x , y, fill= z))+
      geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
      geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2, position=position_dodge(.9)) +
      geom_text(aes_string(label= sig, y = data$mean + data$ste), colour="black", size=3, vjust=-.5, angle = 0, position=position_dodge(.9))+
      scale_y_continuous( ylab , limits = lmt, breaks= brk) +
      scale_x_discrete( xlab)+
      scale_fill_hue(lgl)+
      theme_bw()+
      theme(
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=11, angle=90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = lgd, 
        legend.title = element_text(face="bold", size=10), 
        legend.text = element_text(size=10),
        legend.key.size = unit(1.2, "lines"),
        legend.key = element_blank()
      )
    
  }
  
  else if(type == "line"){
    
    lp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
      geom_line()+
      geom_point(size=2)+ 
      geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2)+
      geom_text(aes_string(label= sig, y = data$mean), colour="black", size=3, vjust=-.5, hjust = -.5,angle = 0)+
      scale_color_discrete(lgl)+
      scale_shape_discrete(lgl)+
      scale_y_continuous(ylab, limits = lmt, breaks= brk)+
      scale_x_discrete(xlab)+
      theme_bw()+
      theme(
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y = element_text(face="bold", size=11, angle=90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = lgd, 
        legend.title = element_text(face="bold", size=10), 
        legend.text = element_text(size=10),
        legend.key.size = unit(1.2, "lines"),
        legend.key = element_blank()
      )
    
  }
  
}








