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



# -------------------------------------------------------------------------
# New graph function ------------------------------------------------------
# Developing version ------------------------------------------------------

#' Plot line or bar graphic
#'
#' @description Function for present the results in line or bar plot
#' @param data Output dtsm fuction
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param x_lab Title for the axis x
#' @param y_lab Title for the axis y
#' @param g_lab Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sig Significance of the result (letters)
#' @param erb Show the error bar.
#' @param y_lmt limits of the y axis
#' @param y_brk break of the y axis
#' @param x_brk axis brakes labels in strign with doble space
#' @param g_brk legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param ang x text label angle
#' @param font letter size in plot
#' @return Line o bar plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete
#' @importFrom gtools mixedsort
#' @export

plot_gr <- function(data, type= "bar", x, y, group, x_lab = NULL, y_lab = NULL, g_lab = NULL, lgd = "top", sig = NULL, erb = FALSE, y_lmt = NULL, y_brk = NULL, x_brk = NULL, g_brk = NULL, color = TRUE, ang = 0, font = 1.5){
  
  ste <- NULL #To avoid this NOTE: fplot: no visible binding for global variable 'ste'
  
  if(is.null(sig)){
    
    data
    
  } else{
    
    data <- data %>% mutate(ymax = mean+ste)
    
  }
  
  if(is.null(y_brk)){
    
    y_brk <- ggplot2::waiver() } else {
      
      y_brk <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20)))*y_brk
      
    }
  
  data[,group] <- factor(data[,group], levels = gtools::mixedsort(levels(as.factor(data[, group]))))
  
  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  
  if ( is.null(y_lab)){
    
    y_lab <- y
    
  } else {
    
    yl <- gsub(pattern = " ",replacement = "~", y_lab)
    y_lab <- eval(expression(parse(text = yl)))
    
  }
  
  if (is.null(x_lab)){
    
    x_lab <- x
    
  } else {
    
    xl <- gsub(pattern = " ",replacement = "~", x_lab)
    x_lab <- eval(expression(parse(text = xl)))
  }
  
  if ( is.null(g_lab)){
    
    g_lab <- group
    
  } else {
    
    
    ll <- gsub(pattern = " ",replacement = "~", g_lab)
    g_lab <- eval(expression(parse(text = ll)))
    
  }
  
  if( !is.null(x_brk)){
    
    x_brk <- unlist(strsplit(x_brk, split = "  "))
    x_brk <- factor(unique(x_brk[x_brk != "  "]))
    x_brk <- as.character(x_brk)
    
  } else {
    
    x_brk <- ggplot2::waiver()
    
  }
  
  if( !is.null(g_brk) ){
    
    g_brk <- unlist(strsplit(g_brk, split = "  "))
    g_brk <- factor(unique(g_brk[g_brk != "  "]))
    g_brk <- as.character(g_brk)
    
  } else {
    
    g_brk <- ggplot2::waiver()
    
  }
  
  if (type == "bar"){
    
    bsp <- ggplot(data, aes_string(x , y, fill= group))+
      geom_col(position = position_dodge2(0.9, preserve = "single"), colour="black", size=.4) +
      scale_x_discrete(x_lab, labels = x_brk)+
      
      if ( color == TRUE ){
        
        scale_fill_discrete(g_lab, labels = g_brk)
        
        
      } else if ( color == FALSE ) {
        
        scale_fill_grey(g_lab, labels = g_brk, start = 1, end = 0.1)
        
      }
    
    if (is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk)
      
    }
    
    if ( !is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, limits = y_lmt, breaks = y_brk, expand = c(0,0))
      
    }
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        
        geom_errorbar(
          aes(ymin = mean - ste, ymax = mean + ste),
          position = position_dodge(0.9),
          width = 0.2) +
        
        geom_text(
          aes_string(label = sig, y = "ymax"),
          position = position_dodge(0.9),
          vjust = -0.5, size= 2*font)
      
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(
          aes(ymin = mean - ste, ymax = mean + ste),
          position = position_dodge(0.9),
          width = 0.2)
      
    }
    
    if ( erb == FALSE && !(is.null(sig)) ){
      
      p <- gr +
        geom_text(
          aes_string(label = sig, y = "ymax"),
          position = position_dodge(0.9),
          vjust = -0.5, size= 2*font)
      
    }
    
    if ( erb == FALSE && is.null(sig) ) {
      
      p <- gr
      
    }
    
  } else if(type == "line"){
    
    if ( color == TRUE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = group, shape= group, color= group))+
        geom_line(size = 0.4)+
        geom_point(size = 1.3*font)+
        scale_x_discrete(x_lab, labels = x_brk)+
        scale_color_discrete(g_lab, labels = g_brk)+
        scale_shape_manual(g_lab, labels = g_brk, values = 1:nlevels(data[,group]))
      
      
    } else if (color == FALSE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = group, shape= group, color= group))+
        geom_line(size = 0.4)+
        geom_point(size = 1.3*font)+
        scale_x_discrete(x_lab, labels = x_brk)+
        scale_color_grey(g_lab, labels = g_brk, start = 0, end = 0) +
        scale_shape_manual(g_lab, labels = g_brk, values = 1:nlevels(data[,group]))
      
    }
    
    if (is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk)
      
    }
    
    if ( !is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk, limits = y_lmt, expand = c(0,0))
      
    }
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), width=.2)+
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), width=.2)
      
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
      axis.title.y = element_text(size= 8*font),
      axis.text.x = element_text(angle = ang),
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


