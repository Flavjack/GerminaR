# GerminaQuant -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(agricolae)
library(GerminaR)



shinyServer(function(input, output) {

  
# User Manual ------------------------------------------------------------

  output$gb<-renderUI({
    
    getPage<-function() {
      return(includeHTML("gb.html"))
      
      
    }
    
    
    getPage()
    
  })
  
  
  
# import data -----------------------------------------------------------

data_fb <-  eventReactive(input$reload, {


  validate(

    need( input$fbdt, message = "Insert a Google spreadsheet URL or xlsx file" )

    )


  if ( !is.null(input$impdata) ) {

    xls <- input$impdata

    file.rename(xls$datapath, paste(xls$datapath, ".xlsx", sep = ""))

    GerminaR::ger_getdata(dir = paste(xls$datapath, ".xlsx", sep = ""), sheet = input$sheetdt)


  } else {

    url <- input$fbdt

    GerminaR::ger_getdata(dir = url)

  }



    }, ignoreNULL = FALSE)



output$fbook <- renderUI({

  gss <- tags$iframe(src = input$fbdt,
    style="height:450px; width:100%; scrolling=no")

  print(gss)

})


# Filter ------------------------------------------------------------------

output$filter_01 <- renderUI({

  file <- data_fb()
  fbn <- names(file)

  selectInput(
    inputId = "filter_nm01",
    label = "Filter 1",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact01 <- renderUI({

  validate(

    need( input$filter_nm01, "Select your levels")

  )

  file <- data_fb()
  fl <- file[, input$filter_nm01]

  selectInput(
    inputId = "filter_ft01",
    label = "Levels",
    choices = c("choose" = "", fl),
    multiple = TRUE
  )

})

output$filter_02 <- renderUI({

  file <- data_fb()
  fbn <- names(file)

  selectInput(
    inputId = "filter_nm02",
    label = "Filter 2",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact02 <- renderUI({

  validate(

    need( input$filter_nm02, "Select your levels")

  )

  file <- data_fb()
  fl <- file[, input$filter_nm02]

  selectInput(
    inputId = "filter_ft02",
    label = "Levels",
    choices = c("choose" = "", fl),
    multiple = TRUE
  )

})


# Data analisys -----------------------------------------------------------


fb <- reactive({


  file <- data_fb()

  fc1 <- input$filter_nm01
  lv1 <- input$filter_ft01

  fc2 <- input$filter_nm02
  lv2 <- input$filter_ft02


  if( fc1 == "" && fc2 == "" ){


  dt <- file


  } else if (fc1 != "" && fc2 != ""){

    dt <- file %>%
      subset( eval(parse(text = fc1)) %in% lv1 & eval(parse(text = fc2)) %in% lv2 )


        if ( length(lv1) == 1){

          dt[, fc1] <- NULL
        }

        if ( length(lv2) == 1){

          dt[, fc2] <- NULL
        }

  } else if (fc1 != "" && fc2 == "" ){


    dt <- file %>%
      subset( eval(parse(text = fc1)) %in% lv1 )


          if ( length(lv1) == 1){

            dt[, fc1] <- NULL
          }

  } else if (fc1 == "" && fc2 != "" ){

    dt <- file %>%
      subset( eval(parse(text = fc2)) %in% lv2 )


          if ( length(lv2) == 1){

            dt[, fc2] <- NULL
          }

  }


  dt


})


# Index Caculation --------------------------------------------------------


varCal <- reactive({
  inFile <- fb()
  if (is.null(inFile )) return(NULL)
  GerminaR::ger_summary(SeedN = input$SeedN , evalName = input$evalName , data = inFile  )
})



output$summary = DT::renderDataTable({
  
  file <- varCal()
  
  file <- file %>% format(digits = 3, nsmall = 3)
  
  
  DT::datatable(file,
                
                filter = 'top',
                extensions = c('Buttons', 'Scroller'),
                rownames = FALSE,
                
                options = list(
                  
                  searchHighlight = TRUE,
                  searching = TRUE,
                  
                  dom = 'Bfrtip',
                  buttons = list(
                    'copy',
                    list(extend = 'csv', filename = input$stat_rsp),
                    list(extend = 'excel', filename = input$stat_rsp)
                  ),
                  
                  autoWidth = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets ="_all")),
                  deferRender=TRUE,
                  scrollY = 400,
                  scrollX = TRUE,
                  scroller = TRUE,
                  
                  initComplete = DT::JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
                ))
  
})


# boxplot -----------------------------------------------------------------

output$bpx <- renderUI({


  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "xbp",
    label = "Axis X",
    choices = c("choose" = "", fbn)
  )

})

output$bpy <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "ybp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$bpz <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "zbp",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})


output$boxplot <- renderPlot({

  validate(

    need( input$ybp, "Select your response variable"),
    need( input$xbp, "Select your X axis variable" ),
    need( input$zbp, "Select your grouped variable")

  )



  file <- varCal()


  variable <- input$ybp
  fx <-  input$xbp
  fz <-  input$zbp
  gply <- input$bply
  gplx <- input$bplx
  gplz <- input$bplz
  brk <- input$bpbrk

  # Title axis --------------------------------------------------------------

  if ( gply == ""){

    gply <- NULL

  }

  if ( gplx == ""){

    gplx <- NULL

  }


  if ( gplz == ""){

    gplz <- NULL

  }


  if(is.na(brk)){

    brks <- NULL

  } else { brks <- brk}


  boxp <- GerminaR::ger_boxp(

    data = file,
    y = variable,
    x = fx,
    z = fz,
    xlab = gplx,
    ylab = gply,
    lgl =  gplz,
    lgd = "top",
    font = input$bpsize,
    brk = brks

  )

  boxp


})


# multivariate ------------------------------------------------------------

# output$crpt <- renderPlot({
# 
#   file <- varCal()
# 
#   fieldbook::plot_correlation(
#     data = file,
#     sig = input$corsig,
#     color = input$corcol,
#     font = input$cor_font)
# 
# })
# 
# 
# output$pca <- renderPlot({
# 
#   file <- varCal()
# 
# 
#   if( is.na(input$pcaqs) ){
# 
#     qs <- NULL
# 
#   } else {
# 
#     qs <- input$pcaqs
# 
#   }
# 
# 
#   if( input$pcalbl == "" ){
# 
#     lbl <- NULL
# 
#   } else {
# 
#     lbl <- input$pcalbl
# 
#   }
# 
# 
# 
# 
#   fieldbook::plot_PCA(
#     data = file,
#     type = input$pcatype,
#     quali.sup = qs,
#     lgl = lbl
#     )
# 
# 
# })


# statistics --------------------------------------------------------------

# Select factors


output$stat_response <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "stat_rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stat_factor <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "stat_fact",
    label = "Factors",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})



output$stat_block <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "stat_blk",
    label = "Block",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})

# ANOVA


av <- reactive({

  validate(

    need( input$stat_rsp, "Select your response variable" ),
    need( input$stat_fact, "Select your factors")

  )

    file <- varCal()

    variable <- input$stat_rsp

    factor <- input$stat_fact %>% paste0() %>%  paste(collapse= " * ")

    block <- input$stat_blk %>% paste0() %>% paste(collapse= " + ")

    file <- file %>% dplyr::mutate_each_(funs(factor(.)), input$stat_fact)


    if ( block == "" ){

      formula <- as.formula(paste( variable , factor, sep = " ~ "))


    } else {

      formula <- as.formula(paste( variable , paste(block, factor, sep = " + "), sep = " ~ "))

    }


    av <- aov(formula, data = file)
    av



})


# ANOVA table

output$tbav = renderPrint({

  file <- av()

  summary(file)


})


# comparison test


comp <- reactive({


  file <- av()
  test <- input$stmc
  sig <- input$stsig
  factor <- input$stat_fact
  variable <- input$stat_rsp


  if( length(factor) == 1 && !(variable == '') )

  {

    rs <- GerminaR::ger_testcomp(
      aov = file,
      comp = factor[1],
      type = test,
      sig = sig)


  }


  else if( length(factor) >= 2 && !(variable == '') )

  {

    rs <- GerminaR::ger_testcomp(
      aov = file,
      comp = c( factor[1], factor[2] ),
      type = test,
      sig = sig)


  }


  rs


})



# Mean comparison table

output$mnc = DT::renderDataTable({

  file <- comp()

  file <- file %>% format(digits = 3, nsmall = 3)


  DT::datatable(file,

    filter = 'top',
    extensions = c('Buttons', 'Scroller'),
    rownames = FALSE,

    options = list(

      searchHighlight = TRUE,
      searching = TRUE,

      dom = 'Bfrtip',
      buttons = list(
        'copy',
        list(extend = 'csv', filename = input$stat_rsp),
        list(extend = 'excel', filename = input$stat_rsp)
        ),

      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets ="_all")),
      deferRender=TRUE,
      scrollY = 400,
      scrollX = TRUE,
      scroller = TRUE,

      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ))

})


# graphics ----------------------------------------------------------------

stat_plot <- reactive({

df <- comp()

factor <- input$stat_fact
variable <- input$stat_rsp

gtype <- input$gtype
gcolor <- input$gcolor

gply <- input$gply
gplx <- input$gplx
gplz <- input$gplz

gerbr <- input$gerbr
gsig <- input$gsig
gfont <- input$gfont
glabel <- input$glabel

limits <- input$glmti * input$glmtf
brakes <- input$gbrakes

xbl <- input$gp_xbk
zbl <- input$gp_zbk


# Title axis --------------------------------------------------------------

if ( gply == ""){

  gply <- variable

} else {

  gply <- input$gply

}

if ( gplx == ""){

  gplx <- NULL

}

if ( gplz == ""){

  gplz <- NULL

}


# Color -------------------------------------------------------------------

if ( gcolor == "yes" ){

  gcolor <- TRUE

} else {

  gcolor <- FALSE

}



# Label brake axis --------------------------------------------------------


if ( xbl == ""){

  xbl <- NULL

} else {

  xbl <- input$gp_xbk

}

if ( zbl == ""){

  zbl <- NULL

} else {

  zbl <- input$gp_zbk

}

# limits & brake ----------------------------------------------------------

if(is.na(limits)) {

  glimits <- NULL

} else {

  glimits <- c(input$glmti, input$glmtf)

}


if(is.na(brakes)) {

  gbrakes <- NULL

} else {

  gbrakes <- brakes

  }


# Error & significance ----------------------------------------------------

if(gerbr == "yes"){

  gerbr <- TRUE

}

if (gerbr == "no"){

  gerbr <-  FALSE

  }


if(gsig == "yes"){

  gsig <- "sg"

}

if (gsig == "no"){

  gsig <-  NULL

  }



# body graph --------------------------------------------------------------


if( length(factor) == 1 && !(variable == '') ){

         pt <- GerminaR::fplot(data = df, type = gtype,
                             x = factor[1],
                             y = "mean",
                             z = factor[1],
                             ylab = gply,
                             xlab = gplx,
                             lgl = gplz,
                             lgd = glabel,
                             erb = gerbr,
                             sig = gsig,
                             font = gfont,
                             lmt = glimits,
                             brk = gbrakes,
                             xbl = xbl,
                             zbl = zbl,
                             color = gcolor)


}


else if( length(factor) >= 2  && !(variable == ''))

{


  pt <- GerminaR::fplot(data = df, type = gtype,
    x = factor[1],
    y = "mean",
    z = factor[2],
    ylab = gply,
    xlab = gplx,
    lgl = gplz,
    lgd = glabel,
    erb = gerbr,
    sig = gsig,
    font = gfont,
    lmt = glimits,
    brk = gbrakes,
    xbl = xbl,
    zbl = zbl,
    color = gcolor
  )


}


pt


})



# plot output -------------------------------------------------------------

output$stplot <- renderPlot({

  plot <-  stat_plot()
  plot

})

# download plot -----------------------------------------------------------

output$download_plot <- downloadHandler(
  file = function(){ paste( "plot_", input$stat_rsp, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = stat_plot(), device = "tiff", dpi = 300, width = input$plot_W, height = input$plot_H, units = "mm" )

  }
)



# Lineal regression -------------------------------------------------------

output$lrg_variable1 <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_var1",
    label = "Variable",
    choices = c("choose" = "", fbn)
  )

})


output$lrg_variable2 <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_var2",
    label = "Variable",
    choices = c("choose" = "", fbn)
  )

})



output$lrg_grouped <- renderUI({

  file <- varCal()
  fbn <- names(file)

  selectInput(
    inputId = "lrg_group",
    label = "Grouped",
    choices = c("choose" = "", fbn)
  )

})


plot_lr <- reactive({

  validate(

    need( input$lrg_var1, "Select your numeric variable"),
    need( input$lrg_var2, "Select your numeric variable" )

  )


  file <- varCal()
  xvr <- input$lrg_var1
  yvr <- input$lrg_var2
  zvr <- input$lrg_group
  sfn <- input$lr_font
  col <- input$lr_color
  lgp <- input$lr_label
  xlab <- input$lr_lbv1
  ylab <- input$lr_lbv2
  lgl <- input$lr_lbgp
  xbk  <- input$lr_brk1
  ybk <- input$lr_brk2
  lvl <- input$lr_lglv
  rlx <- input$lr_eq_x
  rly <- input$lr_eq_y


  if ( col == "yes" ){

    col <- TRUE

  } else {

    col <- FALSE

  }

  if ( zvr == "" ){

    zvr <- NULL

  }

  if ( ylab == "" ){

    ylab <- NULL

  }

  if ( xlab == "" ){

    xlab <- NULL

  }


  if ( lgl == "" ){

    lgl <- NULL

  }

  if ( lvl == "" ){

    lvl <- NULL

  }

  if ( is.na(ybk) ){

    ybk <- NULL

  }

  if ( is.na(xbk) ){

    xbk <- NULL

  }

  if ( is.na(rlx) ){

    rlx <- NULL

  }

  if ( is.na(rly) ){

    rly <- NULL

  }

  GerminaR::ger_linereg(
    data = file,
    y = yvr,
    x = xvr,
    z = zvr,
    lgd = lgp,
    color = col,
    ylab = ylab,
    xlab =  xlab,
    lgl = lgl,
    xbrk = xbk,
    ybrk = ybk,
    zbl = lvl,
    font = sfn,
    rlx = rlx,
    rly = rly
  )


})



output$plot_regression <- renderPlot({

  plot <-  plot_lr()
  plot

})


# download reg plot -----------------------------------------------------------

output$download_plot_lr <- downloadHandler(
  file = function(){ paste( "plot_", input$lrg_var2, "_" ,  input$lrg_var1, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = plot_lr(), device = "tiff", dpi = 300, width = input$lr_plot_W, height = input$lr_plot_H, units = "mm" )

  }
)


# Germination InTime ------------------------------------------------------


output$smvar <- renderUI({
  inFile <- fb()


  if (is.null(inFile)) return(NULL)

  evf <- GerminaR::evalFactor(evalName = input$evalName , data = inFile)

  selectInput('smvr', 'Summarize by', c(Choose='', names(evf)))
})




gnt <- reactive({
  inFile <- fb()
  if (is.null(inFile)) { return(NULL) }
  else if (input$smvr ==''){ return(NULL) }
  else {

    smt <- GerminaR::ger_intime( input$smvr, input$SeedN , input$evalName, input$git_type, inFile)

  }

})


output$gertime <- renderTable({

  gnt()

})


plot_git <- reactive({
  
  
  df <- gnt()
  
  df[, "evaluation"] <- factor(df[,"evaluation"], levels = gtools::mixedsort(levels(as.factor(df[,"evaluation"]))))
  df[,input$smvr] <- factor(df[,input$smvr], levels = gtools::mixedsort(levels(as.factor(df[,input$smvr]))))
  
  if (is.null(df)) return(NULL)
  else if (input$smvr =='' ){ return(NULL) }
  else{
    
    
    gcolor <- input$git_color
    
    gply <- input$git_ly
    gplx <- input$git_lx
    gplz <- input$git_lz
    
    gfont <- input$git_font
    glabel <- input$git_label
    
    limits <- input$git_lmti * input$git_lmtf
    brakes <- input$git_brakes
    
    xbl <- input$git_xbk
    zbl <- input$git_zbk
    
    
    # Title axis --------------------------------------------------------------
    
    if ( gplz == ""){
      
      gplz <- NULL
      
    }
    
    
    # Color -------------------------------------------------------------------
    
    if ( gcolor == "yes" ){
      
      gcolor <- TRUE
      
    } else {
      
      gcolor <- FALSE
      
    }
    
    
    
    # Label brake axis --------------------------------------------------------
    
    
    if ( xbl == ""){
      
      xbl <- NULL
      
    } else {
      
      xbl <- input$git_xbk
      
    }
    
    if ( zbl == ""){
      
      zbl <- NULL
      
    } else {
      
      zbl <- input$git_zbk
      
    }
    
    # limits & brake ----------------------------------------------------------
    
    if(is.na(limits)) {
      
      glimits <- NULL
      
    } else {
      
      glimits <- c(input$git_lmti, input$git_lmtf)
      
    }
    
    
    if(is.na(brakes)) {
      
      gbrakes <- NULL
      
    } else {
      
      gbrakes <- brakes
      
    }
    
    
    
    
    GerminaR::fplot(data = df, 
                    type = "line", 
                    x = "evaluation", 
                    y = "mean", 
                    z = input$smvr, 
                    ylab = gply, 
                    xlab = gplx, 
                    lgl = gplz, 
                    font = gfont, 
                    lmt = glimits, 
                    brk = gbrakes,
                    color = gcolor, 
                    lgd = glabel,
                    xbl = xbl,
                    zbl = zbl
    )
    
    
  }
  
  
})


output$GerInTime = renderPlot({

  
  plot <-  plot_git()
  plot
  
})

# download git plot -----------------------------------------------------------

output$download_plot_git <- downloadHandler(
  file = function(){ paste( "plot_git_", input$smvr, '.tiff', sep = '')},
  content = function(file){
    ggplot2::ggsave(file, plot = plot_git(), device = "tiff", dpi = 300, width = input$git_plot_W, height = input$git_plot_H, units = "mm" )
    
  }
)


# osmotic tools -----------------------------------------------------------


output$ops <- reactive({
  
  if( input$pre > 0) return( "presure should be negative")
  else{  
    
    r1 <- (-0.00820574587 * (input$tem + 273) * input$dis)
    
    r2 <- input$pre/r1
    
    r3 <- r2 * 1000
    
    r4 <- (input$psm * r3 * input$vol*1000)/10^6
    
    round(r4, 5)
    
  }
  
}) 


output$opp <- reactive({
  
  if( input$prep > 0) return( "presure should be negative")
  else{  
    
    mpb <- input$prep * 10
    
    C <- (-1.18 * 10^(-2))
    
    C2 <- (-1.18 * 10^(-4))
    
    CT <- (2.67 * 10^(-4)) * input$temp
    
    C2T <- (8.39 * 10^(-7)) * input$temp
    
    b <- (C + CT) * (-1)
    
    a <- (C2 + C2T) * (-1)
    
    c <- mpb
    
    b2 <- b^2
    
    ac4 <- (4*a*c*(-1)) 
    
    delta <- b2 + ac4
    
    srdt <- abs(sqrt(delta))
    
    round(srdt, 5)
    
  }
  
})



})
