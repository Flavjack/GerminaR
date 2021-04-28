#' Boxplot graphic
#'
#' @description Function use the raw data for made a boxplot graphic
#' @param data raw data
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param ylimits Limitis and break of the y axis c(init, end, brakes)
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis
#' @param gtext Text labels in groups
#' @param opt Add news layer to the plot
#' @return boxplot
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(GerminaR)
#'
#' fb <- ger_summary(SeedN = "seeds", evalName = "D", data = prosopis)
#'
#' ger_boxp(data = fb
#'          , x =  "nacl"
#'          , y = "grp"
#'          , group = "temp"
#'          )
#'
#' }
#' 

ger_boxp <- function(data
                     , x
                     , y
                     , group = NULL
                     , xlab = NULL
                     , ylab = NULL
                     , glab = NULL
                     , ylimits = NULL
                     , xrotation = NULL
                     , legend = "top"
                     , xtext = NULL
                     , gtext = NULL
                     , opt = NULL
                     ){

# test --------------------------------------------------------------------

  if(FALSE) {
    
    library(GerminaR)

    fb <- prosopis %>% 
      ger_summary(SeedN = "seeds"
                  , evalName = "D") 
    
    x <- "temp" 
    group <- "nacl"
    y <- "grp"
    xlab <- NULL # "label x"
    ylab <- NULL # "label y"
    glab <- NULL # "legend"
    ylimits <- NULL # c(0, 120, 10)
    xrotation <- NULL #c(0, 0.5, 0.5)
    legend <- "top"
    gtext <- NULL
    xtext <- NULL
    
    data <- fb
    
  }
  
# -------------------------------------------------------------------------

  if(!c(x %in% colnames(data))) stop("colum no exist")
  if(!c(y %in% colnames(data))) stop("colum no exist")
  
  if(is.null(xrotation)) xrotation <- c(0, 0.5, 0.5) 
  if(is.null(group)) group <- x else group <- group 
  
  plotdt <- data %>% 
    mutate(across(c({{x}}, {{group}}), as.factor))

  plot <- plotdt %>% 
    ggplot(., aes(x = .data[[x]]
                  , y = .data[[y]]
                  , fill = .data[[group]]
           )) +
    geom_boxplot(outlier.colour = "red", outlier.size = 2.5) +
    geom_point(position = position_jitterdodge()) + {
      
      if(!is.null(ylimits)) {
        scale_y_continuous(
          limits = ylimits[1:2] 
          , breaks = seq(ylimits[1], ylimits[2], by = ylimits[3])
          , expand = c(0,0)
        ) 
      }
      
    } +
    labs(
      x = if(is.null(xlab)) x else xlab
      , y = if(is.null(ylab)) y else ylab
      , fill = if(is.null(glab)) group else glab
    ) + 
  {if(!is.null(gtext)) scale_fill_discrete(labels = gtext)} + 
  {if(!is.null(xtext)) scale_x_discrete(labels = xtext)} 
  

layers <- 'plot +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent")
      , plot.background = element_rect(fill = "transparent")
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , legend.background = element_rect(fill = "transparent")
      , legend.box.background = element_rect(fill = "transparent")
      , legend.position = legend
      , axis.text.x = element_text(angle = xrotation[1]
                                   , hjust= xrotation[2]
                                   , vjust = xrotation[3])
      )'

if(is.null(opt)) {
  eval(parse(text = layers)) 
} else {
  eval(parse(text = paste(layers, opt, sep = " + ")))
}

}
