#' Plot line or bar graphic
#' 
#' @description Function use the dtsm funtion for plot the results 
#' @param data Output dtsm fuction
#' @param type Type of graphic. "bar" or "line" 
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y 
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sig Significance of the result (letters)
#' @param erb Show the error bar.
#' @param lmt limits of the y axis
#' @param brk break of the y axis
#' @return Line o bar plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit
#' @export

fplot <- function(data, type= "line", x, y, z, ylab = "", xlab = "", lgl = "",lgd = "right", sig = "",erb = FALSE, lmt = NULL, brk = ggplot2::waiver()){
  
 ste <- NULL #To avoid this NOTE: fplot: no visible binding for global variable 'ste'
  
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))
  z <- deparse(substitute(z))
  sig <- deparse(substitute(sig)) 
  
  data[,x] <- factor(data[,x], levels = gtools::mixedsort(data[,x]))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(data[,z]))
  
  
  yl <- gsub(pattern = " ",replacement = "~", ylab)
  ylab <- eval(expression(parse(text = yl)))
  
  xl <- gsub(pattern = " ",replacement = "~", xlab)
  xlab <- eval(expression(parse(text = xl)))

  ll <- gsub(pattern = " ",replacement = "~", lgl)
  lgl  <- eval(expression(parse(text = ll)))
  
  
  data <- data %>% mutate(ymax = mean+ste)
  
  if (type == "bar" & erb == TRUE){
    
    bp <- ggplot(data, aes_string(x , y, fill= z))+
      geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
      geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2, position=position_dodge(.9)) +
      geom_text(aes_string(label= sig, y = "ymax"), colour="black", size=3, vjust=-.5, angle = 0, position=position_dodge(.9))+
      scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brk) +
      scale_x_discrete(xlab)+
      scale_fill_discrete(lgl)
    
  } else if(type == "bar" & erb == FALSE){
    
    bp <- ggplot(data, aes_string(x , y, fill= z))+
      geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
      geom_text(aes_string(label= sig, y = y), colour="black", size=3, vjust=-.5, angle = 0, position=position_dodge(.9))+
      scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brk) +
      scale_x_discrete(xlab)+
      scale_fill_discrete(lgl)
    
  } else if (type == "line" & erb == TRUE){
    
    bp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
      geom_line()+
      geom_point(size=2)+ 
      geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2)+
      geom_text(aes_string(label= sig, y = y), colour="black", size=3, vjust=-.5, hjust = -.5,angle = 0)+
      scale_color_discrete(lgl)+
      scale_shape_discrete(lgl)+
      scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brk)+
      scale_x_discrete(xlab)
    
  } else if(type == "line" & erb == FALSE){
    
    bp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
      geom_line()+
      geom_point(size=2)+ 
      geom_text(aes_string(label= sig, y = y), colour="black", size=3, vjust=-.5, hjust = -.5,angle = 0)+
      scale_color_discrete(lgl)+
      scale_shape_discrete(lgl)+
      scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brk)+
      scale_x_discrete(xlab)
    
  }
  
  
  bp + theme_bw()+
    theme(
      axis.title.x = element_text(face="bold", size=10),
      axis.title.y = element_text(face="bold", size=10, angle=90),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = lgd, 
      legend.title = element_text(face="bold", size=10), 
      legend.text = element_text(size=10),
      legend.key.size = unit(1.2, "lines"),
      legend.key = element_blank()
    )
  
  
}
