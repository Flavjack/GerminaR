#' Plot line or bar graphic
#'
#' @description Function use the dtsm function for plot the results
#' @param data Output from ger_testcomp function
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param groups Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param legend the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sig Column with the significance
#' @param error Show the error bar ("ste" or "std").
#' @param limits limits of the y axis
#' @param brakes break of the y axis
#' @param xbrks axis brakes labels in string with double space
#' @param gbrks legend label in string with double space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @return Line o bar plot
#' @import dplyr
#' @importFrom grDevices colorRampPalette colors
#' @importFrom tibble deframe
#' @importFrom grDevices gray.colors
#' @import ggplot2
#' @export
#' 
#' @examples  
#' 
#' \dontrun{
#' 
#' library(GerminaR)
#' library(dplyr)
#' data <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))
#' smr <- ger_summary(SeedN = "seeds", evalName = "D", data = data)
#' 
#' aov <- aov(grp ~ nacl*temp, smr)
#' 
#' mc <- ger_testcomp(aov = aov
#'                    , comp = c("nacl", "temp")
#'                    )
#'                    
#' data <- mc$table
#'                     
#'  fplot(data = data
#'        , type = "line"
#'        , x = "temp"
#'        , y = "grp"
#'        , groups = "nacl"
#'        , limits = c(0,150)
#'        , brakes = 20
#'        , color = FALSE
#'        )
#'                    
#' } 

fplot <- function(data
                  , type = "bar"
                  , x
                  , y
                  , groups
                  , xlab = NULL
                  , ylab = NULL
                  , glab = NULL
                  , legend = "top"
                  , sig = NULL
                  , error = "ste"
                  , limits = NULL
                  , brakes = NULL
                  , xbrks = NULL
                  , gbrks = NULL
                  , color = TRUE
                  ){
  
  # xlab <- ylab <- glab <- NULL
  # limits <- brakes <- legend <- NULL
  # type = "bar"
  # legend = "top"
  # color = TRUE
  
# arguments ---------------------------------------------------------------

  legend <- match.arg(legend, c("top", "left", "right", "bottom", "none"))
  
  type <- match.arg(type, c("barra", "linea"))
  
  plot_dt <- data

# -------------------------------------------------------------------------
  
  if ( !is.null(xlab) ) { 
    
    xlab <- xlab %>%
      gsub(pattern = " ", "~", .)
    xlab <- eval(expression(parse(text = xlab)))
    
  } else { xlab <- x }
  
  if ( !is.null(ylab) ) { #
    
    ylab <- ylab %>%
      gsub(pattern = " ", "~", .)
    
    ylab <- eval(expression(parse(text = ylab)))
    
  } else { ylab <- y }
  
  if ( !is.null(glab) ) {
    
    glab <- glab %>%
      gsub(pattern = " ", "~", .)
    glab <- eval(expression(parse(text = glab)))
    
  } else { glab <- groups }
  
  # -------------------------------------------------------------------------
  
  min_value <- min(plot_dt$min)
  max_value <- max(plot_dt$max)
  
  if ( is.null(brakes) ) { brakes <- abs(round(max_value*1.2, 1))/5 } 
    
  if ( is.null(limits) ) {
    
    if ( min_value >= 0 & max_value > 0 ) {
      
      limits <- paste(0, round(max_value*1.2, 1),  sep = "x")

    } else if ( min_value < 0 &  max_value > 0 ) {
      
      limits <- paste(round(min_value*1.2, 1)
                      , round(max_value*1.2, 1),  sep = "x")

    } else if ( min_value < 0 &  max_value <= 0 ) {
      
      limits <- paste( round(min_value*1.2, 1), 0,  sep = "x")

    }
    
    limits <- limits %>% strsplit(., "x") %>% deframe() %>% as.numeric()

  } 

  if ( limits[1] >= 0 & limits[2] >= 0 ) {
    
    brakes <- ((limits[1]*-100):(limits[2]*+100)) * brakes
    
  } else if ( limits[1] <= 0 &  limits[2] <= 0 ) {
    
    brakes <- ((limits[1]*+100):(limits[2]*-100)) * brakes
    
  } else if ( limits[1] <= 0 & limits[2] >= 0 ) {
    
    brakes <- ((limits[1]*+100):(limits[2]*+100)) * brakes
    
  }
    
# -------------------------------------------------------------------------
  
  if (color != TRUE ) {
    
    color_grps <- gray.colors(n =  plot_dt[[groups]] %>% unique() %>% length()
                          , start = 0.8
                          , end = 0.3) 
  }

# bar plot ----------------------------------------------------------------
# -------------------------------------------------------------------------
  
  barplot <- function(plot_dt
                      , x
                      , y
                      , groups
                      , xlab
                      , ylab
                      , glab
                      , limits
                      , brakes
                      , sig
                      , error
                      , legend
  ) {
    
    plot_dt %>%
      
      {if ( x != groups ) complete(., .data[[groups]], .data[[x]] ) else . } %>%
      
      ggplot( aes( .data[[x]] , .data[[y]], fill = .data[[groups]] ) ) +
      geom_col(
        position = position_dodge2()
        , colour="black"
        , size=.4
        , na.rm = T
        ) +
      
      scale_y_continuous(limits = limits
                         , breaks = brakes
                         , expand = c(0,0)) +
      
      { if (color != TRUE ) scale_fill_manual(values = color_grps) } +
      
      geom_errorbar(
        aes(ymin = .data[[y]] - .data[[error]]
            , ymax = .data[[y]] + .data[[error]] )
        , position = position_dodge(width = 0.9)
        , width = 0.15
        , na.rm = T
      ) +
      
      {if (!is.null(sig))  geom_text(
        aes(label = .data[[sig]],
            y = .data[[y]] + .data[[error]]  )
        , position = position_dodge(width = 0.9)
        , na.rm = T
        , colour = "black"
        , vjust = -0.5
        , hjust = 0.5
        , angle = 0
      ) } +
      
      labs(x = xlab
           , y = ylab
           , fill = glab
      )
  }
  
  # line plot ---------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  lineplot <- function(plot_dt
                       , x
                       , y
                       , groups
                       , xlab
                       , ylab
                       , glab
                       , limits
                       , brakes
                       , sig
                       , error
                       , legend
  ) {
    
    plot_dt %>%
      ggplot( aes( .data[[x]] , .data[[y]]
                   , shape = .data[[groups]]
                   , colour = .data[[groups]]
                   ) ) +
      
      geom_point( aes(group =  .data[[groups]]
                      , shape = .data[[groups]]
                      , color = .data[[groups]]
      ), size = 2.5 ) +
      
      geom_line( aes( group =  .data[[groups]]
                      , color = .data[[groups]]
                      , linetype = .data[[groups]]
      ) ,  size = 1 ) +
      
      scale_y_continuous(limits = limits
                         , breaks = brakes
                         , expand = c(0,0)) +
      
      { if (color != TRUE ) scale_color_manual(values = color_grps) } +
      
      geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                        , ymax = .data[[y]] + .data[[error]])
                    , width = 0.08) +
      
      {if (!is.null(sig))  geom_text(aes(label = .data[[sig]], y = .data[[y]] + .data[[error]])
                                     , colour = "black"
                                     , vjust = -0.5
                                     , hjust = 0.5
                                     , angle = 0) } +
      
      labs(x = xlab, y = ylab
           , shape = glab, color = glab, linetype = glab)
    
  }
  
  # apply functions----------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if ( type == "barra" ) {
    
    plot <- barplot(plot_dt
                    , x
                    , y
                    , groups
                    , xlab
                    , ylab
                    , glab
                    , limits
                    , brakes
                    , sig
                    , error
                    , legend
                    )
  }
  
  if ( type == "linea" ) {
    
    plot <- lineplot(plot_dt
                     , x
                     , y
                     , groups
                     , xlab
                     , ylab
                     , glab
                     , limits
                     , brakes
                     , sig
                     , error
                     , legend 
                     )
  }
  
  # results -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  plot +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      legend.position = legend
    )
  
}

