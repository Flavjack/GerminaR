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
#' @param xbl axis brakes labels in strign with doble space
#' @param zbl legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param font letter size in plot
#' @return Line o bar plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete
#' @importFrom gtools mixedsort
#' @export

fplot <- function(data, type= "bar", x, y, z, ylab = NULL, xlab = NULL, lgl = NULL,lgd = "top", sig = NULL, erb = FALSE, lmt = NULL, brk = NULL, xbl = NULL, zbl = NULL, color = TRUE, font = 1){
  
  ste <- NULL #To avoid this NOTE: fplot: no visible binding for global variable 'ste'
  
  if(is.null(brk)){
    
    brks <- ggplot2::waiver() } else {
      
      brks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * brk
      
    }
  
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))
  
  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  
  if ( is.null(ylab)){
    
    ylab <- y
 
  } else {
    
    yl <- gsub(pattern = " ",replacement = "~", ylab)
    ylab <- eval(expression(parse(text = yl)))
    
  }
  
  if ( is.null(xlab)){
    
    xlab <- x
    
  } else {
    
    
    xl <- gsub(pattern = " ",replacement = "~", xlab)
    xlab <- eval(expression(parse(text = xl)))
    
  }
  
  if ( is.null(lgl)){
    
    lgl <- z
    
  } else {
    
    
    ll <- gsub(pattern = " ",replacement = "~", lgl)
    lgl  <- eval(expression(parse(text = ll)))
    
  }
  
  data <- data %>% mutate(ymax = mean+ste)
  
  if( !is.null(xbl) ){
    
    xbl <- unlist(strsplit(xbl, split = "  "))
    xbl <- factor(unique( xbl[ xbl != "  "]))
    xbl <- as.character(xbl)
    
  } else {
    
    xbl <- ggplot2::waiver()
    
  }
  
  if( !is.null(zbl) ){
    
    zbl <- unlist(strsplit(zbl, split = "  "))
    zbl <- factor(unique( zbl[ zbl != "  "]))
    zbl <- as.character(zbl)
    
  } else {
    
    zbl <- ggplot2::waiver()
    
  }
  
  if (type == "bar"){
    
    bsp <- ggplot(data, aes_string(x , y, fill= z))+
      geom_bar(position=position_dodge(),colour="black",stat="identity", size=.4)+
      scale_x_discrete(xlab, labels = xbl)+
      
      if ( color == TRUE ){
        
        scale_fill_discrete(lgl, labels = zbl)
        
      } else if ( color == FALSE ) {
        
        scale_fill_grey(lgl, labels = zbl, start = 1, end = 0.1)
        
      }
    
    
    if (is.null(lmt)){
      
      gr <- bsp + scale_y_continuous(ylab, breaks = brks)
      
    }
    
    if ( !is.null(lmt)){
      
      gr <- bsp + scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brks)
      
    }
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2, position=position_dodge(.9)) +
        geom_text(aes_string(label= sig, y = "ymax"), colour="black", size= 2*font, vjust=-.5, angle = 0, position=position_dodge(.9))
      
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2, position=position_dodge(.9))
      
    }
    
    if ( erb == FALSE && !(is.null(sig)) ){
      
      p <- gr +
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, angle = 0, position=position_dodge(.9))
      
    }
    
    if ( erb == FALSE && is.null(sig) ) {
      
      p <- gr
      
    }
    
    
  } else if(type == "line"){
    
    if ( color == TRUE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
        geom_line(size = 0.3)+
        geom_point(size = 1.2*font)+
        scale_x_discrete(xlab, labels = xbl)+
        scale_color_discrete(lgl, labels = zbl)+
        scale_shape_discrete(lgl, labels = zbl)
    
    } else if (color == FALSE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
        geom_line(size = 0.3)+
        geom_point(size = 1.2*font)+
        scale_x_discrete(xlab, labels = xbl)+
        scale_color_grey(lgl, labels = zbl, start = 0, end = 0) +
        scale_shape_discrete(lgl, labels = zbl)
      
    }
    
    if (is.null(lmt)){
      
      gr <- bsp + scale_y_continuous(ylab, breaks = brks)
      
    }
    
    if ( !is.null(lmt)){
      
      gr <- bsp + scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brks)
      
    }
    
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2)+
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)
      
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2)
    
    }
    
    if ( erb == FALSE && !(is.null(sig)) ){
      
      p <- gr +
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)
      
    }
    
    if ( erb == FALSE && is.null(sig) ) {
      
      p <- gr
      
    }
    
  }

  p + 
    theme_bw()+
    theme(
      axis.title.x = element_text(size= 8*font),
      axis.title.y = element_text(size= 8*font, angle=90),
      panel.background = element_rect(fill = "transparent"), 
      plot.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent"),
      legend.position = lgd,
      legend.title = element_text(size= 8*font),
      legend.text = element_text(size= 8*font),
      legend.key.size = unit(0.8*font, "lines"),
      legend.key = element_blank(),
      text = element_text(size = 8*font)
    )
}


#' Boxplot graphic
#'
#' @description Function use the raw data for made a boxplot graphic
#' @param data raw data
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param brk break of the y axis
#' @param font letter size in plot
#' @return boxplot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_boxplot geom_errorbar geom_line geom_point geom_text ggplot position_dodge position_jitterdodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete
#' @importFrom gtools mixedsort
#' @export


ger_boxp <- function(data, x, y, z, ylab = NULL, xlab = NULL, lgl = NULL, lgd = "top", brk = NULL, font = 1){
  
  
  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))
  
  
  if( !is.null(xlab) ){
    
    xl <- gsub(pattern = " ",replacement = "~", xlab)
    xlab <- eval(expression(parse(text = xl)))
    
  } else {
    
    xlab <- x
    
  }
  
  if( !is.null(ylab) ){
    
    yl <- gsub(pattern = " ",replacement = "~", ylab)
    ylab <- eval(expression(parse(text = yl)))
    
    
  } else {
    
    ylab <- y
    
  }
  
  
  if( !is.null(lgl) ){
    
    ll <- gsub(pattern = " ",replacement = "~", lgl)
    lgl  <- eval(expression(parse(text = ll)))
    
  } else {
    
    lgl <- z
    
  }
  
  
  if(is.null(brk)){
    
    brks <- ggplot2::waiver() } else {
      
      brks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * brk
      
      
    }
  
  ggplot(data, aes_string( x = x , y = y, fill = z))+
    geom_boxplot(outlier.colour = "red", outlier.size = 2.5)+
    geom_point(position = position_jitterdodge())+
    scale_x_discrete( xlab )+
    scale_y_continuous( ylab, breaks = brks)+
    scale_fill_discrete( lgl )+
    theme_bw()+
    theme(
      axis.title.x = element_text(size= 8*font),
      axis.title.y = element_text(size= 8*font, angle=90),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = lgd,
      legend.title = element_text(size= 8*font),
      legend.text = element_text(size= 8*font),
      legend.key.size = unit(0.8*font, "lines"),
      legend.key = element_blank(),
      legend.background = element_rect(fill= "transparent"),
      text = element_text(size = 8*font)
    )
}


#' Plot line regression
#'
#' @description Function plot linea regression
#' @param data Output dtsm fuction
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param xbrk brakes for x axis
#' @param ybrk brakes for y axis
#' @param zbl legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param font letter size in plot
#' @param rlx regression line position in axis x.
#' @param rly regression line position in axis y.
#' @return Line regression plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_smooth scale_x_continuous scale_color_grey aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete scale_fill_grey annotate
#' @importFrom gtools mixedsort
#' @importFrom stats lm
#' @export


ger_linereg <- function(data, x, y, z = NULL, ylab = NULL, xlab = NULL, lgl = NULL,lgd = "top", xbrk = NULL, ybrk = NULL, zbl = NULL, color = TRUE, font = 1, rlx = NULL, rly = NULL){
  
  if( !is.null(z) ){
    
    data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))
    
  }
  
 
  
  if( !is.null(xlab) ){
    
    xl <- gsub(pattern = " ",replacement = "~", xlab)
    xlab <- eval(expression(parse(text = xl)))
    
  } else {
    
    xlab <- x
    
  }
  
  if( !is.null(ylab) ){
    
    yl <- gsub(pattern = " ",replacement = "~", ylab)
    ylab <- eval(expression(parse(text = yl)))
    
    
  } else {
    
    ylab <- y
    
  }
  
  
  if( !is.null(lgl) ){
    
    ll <- gsub(pattern = " ",replacement = "~", lgl)
    lgl  <- eval(expression(parse(text = ll)))
    
  } else {
    
    lgl <- z
    
  }
  
  
  if( !is.null(zbl) ){
    
    zbl <- unlist(strsplit(zbl, split = "  "))
    zbl <- factor(unique( zbl[ zbl != "  "]))
    zbl <- as.character(zbl)
    
  } else {
    
    zbl <- ggplot2::waiver()
    
  }
  
  
  if(is.null(xbrk)){
    
    xbrks <- ggplot2::waiver() } else {
      
      xbrks <- (((round(mean(data[,x]), 0))*(-20)):((round(mean(data[,x]), 0))*(+20))) * xbrk
      
      
    }
  
  
  if(is.null(ybrk)){
    
    ybrks <- ggplot2::waiver() } else {
      
      ybrks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * ybrk
      
      
    }
  
  rgl <- GerminaR::ger_leq(x, y, data)
  
  if ( is.null(rlx) ){ rlx = -Inf } else { rlx = rlx }
  if ( is.null(rly) ){ rly = Inf } else { rly = rly }
  
  
  p <- ggplot(data, aes_string(  x = x , y = y, group = z, shape= z, color= z))+
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE, size = 0.3)+
    geom_point(size = 1.2*font)+
    scale_x_continuous( xlab, expand = c(0,0), breaks = xbrks)+
    scale_y_continuous( ylab, expand = c(0,0), breaks = ybrks )+
    scale_shape_discrete(lgl, labels = zbl)+
    annotate("text", label = rgl, parse = T, x = rlx, y = rly, vjust = "inward", hjust = "inward", size= 2.5*font)
  
  
  if ( color == TRUE ){
    
    p <- p +
      scale_color_discrete(lgl, labels = zbl)
    
    
    
  } else if (color == FALSE ){
    
    p <- p +
      scale_color_grey(lgl, labels = zbl, start = 0, end = 0)
    
    
  }
  
  p + theme_bw()+
    theme(
      
      panel.border = element_blank(),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      legend.background = element_rect(fill= "transparent"),
      
      legend.title = element_text(size= 8*font),
      legend.text = element_text(size= 8*font),
      legend.key.size = unit(0.8*font, "lines"),
      axis.title.x = element_text(size= 8*font),
      axis.title.y = element_text(size= 8*font, angle=90),
      legend.position = lgd,
      text = element_text(size = 8*font)
    )
}

