# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-10-18
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

if (file.exists("setup.R")) { source("setup.R") }

library(GerminaR)
library(shiny)
library(metathis)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(gsheet)
library(readxl)
library(ggpubr)
library(DT)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
# close auto local session ------------------------------------------------

observe({
  
  if(Sys.getenv('SHINY_PORT') == "") {
    
    session$onSessionEnded(stopApp)
    
  }
  
})
  
# import data -----------------------------------------------------------
  
  observe({
    
    cat("Import data --------------------------------------------------\n")
    
    cat("input$import_excel$name")
    print(input$import_excel$name)
    
    cat("input$import_excel$datapath")
    print(input$import_excel$datapath)
    
    cat("input$import_gsheet")
    print(input$import_gsheet)
    
  })


# -------------------------------------------------------------------------
  
  data_fb <-  eventReactive(input$reload, {
    
    if ( !is.null(input$import_excel) ) {
      
     dt <-  readxl::read_excel(path = input$import_excel$datapath
                 , sheet = input$sheetdt
                 ) %>% as.data.frame()
     
    } else if ( input$import_gsheet != "" ){
      
     dt <- gsheet::gsheet2tbl(url = input$import_gsheet) %>% as.data.frame()
      
    } else { return(NULL) }
    
  }, ignoreNULL = FALSE)


# data viewer -------------------------------------------------------------
# -------------------------------------------------------------------------
  
output$fb_excel <- DT::renderDataTable(server = FALSE, {
    
    DT::datatable(data = data_fb(),
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

# -------------------------------------------------------------------------

output$fb_gsheets <- renderUI({
  
  tags$iframe(src = input$import_gsheet,
              style="height:450px; width:100%; scrolling=no")
  
})
  
output$data_viewer <- renderUI({
  
  if ( !is.null(input$import_excel) & input$import_gsheet != "" ) {
    
    DT::dataTableOutput("fb_excel")

  } else if ( input$import_gsheet != "" ) {
    
    htmlOutput("fb_gsheets")
    
  } else if ( !is.null(input$import_excel) ) {
    
    DT::dataTableOutput("fb_excel")
    
  } else { "Insert a Google spreadsheet URL or xlsx file" }
  
})

# filter ------------------------------------------------------------------

output$filter_01 <- renderUI({
  
  validate( need( data_fb(), "Insert a Google spreadsheet URL or xlsx file") )

  file <- data_fb()
  fbn <- data_fb() %>% select(!starts_with(input$evalName)) %>% names()

  selectInput(
    inputId = "filter_nm01",
    label = "Filter 1 (optional)",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact01 <- renderUI({

  validate( need( input$filter_nm01, "Select your levels") )

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
  
  validate( need( data_fb(), "Insert a Google spreadsheet URL or xlsx file") )

  file <- data_fb()
  fbn <- data_fb() %>% select(!starts_with(input$evalName)) %>% names()

  selectInput(
    inputId = "filter_nm02",
    label = "Filter 2 (optional)",
    choices = c("choose" = "", fbn)
  )

})

output$filter_fact02 <- renderUI({

  validate(need( input$filter_nm02, "Select your levels"))

  file <- data_fb()
  fl <- file[, input$filter_nm02]

  selectInput(
    inputId = "filter_ft02",
    label = "Levels",
    choices = c("choose" = "", fl),
    multiple = TRUE
  )

})

# data filtered -----------------------------------------------------------

fb <- reactive({
  
  validate( need( data_fb(), "Insert a Google spreadsheet URL or xlsx file") )

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

# index calculation --------------------------------------------------------

varCal <- reactive({
  
  validate( need( fb(), "Insert a Google spreadsheet URL or xlsx file") )
  
  inFile <- fb()
  if (is.null(inFile )) return(NULL)
  ger_summary(SeedN = input$SeedN
              , evalName = input$evalName
              , data = inFile
              )

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

var_names <- reactive({
  
 vars <- c("grs", "grp", "mgt", "mgr", "gsp", "unc", "syn", "vgt", "sdg", "cvg")
  
})

output$bpx <- renderUI({

  fbn <- varCal() %>% select(!var_names()) %>% names() 
  
  selectInput(
    inputId = "xbp",
    label = "Axis X",
    choices = c("choose" = "", fbn)
  )
  
})

output$bpy <- renderUI({
  
  fbn <- varCal() %>% select(var_names()) %>% names() 
  
  selectInput(
    inputId = "ybp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )
  
})

output$bpz <- renderUI({
  
  fbn <- varCal() %>% select(!var_names()) %>% names() 
  
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
  
  if ( gply == ""){ gply <- NULL }
  
  if ( gplx == ""){ gplx <- NULL }
  
  if ( gplz == ""){ gplz <- NULL }
  
  if(is.na(brk)){
    
    brks <- NULL
    
  } else { brks <- brk}
  
  boxp <- ger_boxp(data = file
                   , y = variable
                   , x = fx
                   , z = fz
                   , xlab = gplx
                   , ylab = gply
                   , lgl =  gplz
                   , lgd = "top"
                   , font = input$bpsize
                   , brk = brks)
  boxp

})

# statistics --------------------------------------------------------------

# Select factors

output$stat_response <- renderUI({

  fbn <- varCal() %>% select(var_names()) %>% names() 

  selectInput(
    inputId = "stat_rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stat_factor <- renderUI({

  fbn <- varCal() %>% select(!var_names()) %>% names() 

  selectInput(
    inputId = "stat_fact",
    label = "Factors",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})



output$stat_block <- renderUI({

  fbn <- varCal() %>% select(!var_names()) %>% names() 
  
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

    file <- file %>% dplyr::mutate_each_(funs(factor(.)), c(input$stat_fact, input$stat_blk))

    if ( block == "" ){

      formula <- as.formula(paste( variable , factor, sep = " ~ "))


    } else {

      formula <- as.formula(paste( variable , paste(block, factor, sep = " + "), sep = " ~ "))

    }

    av <- aov(formula, data = file)

})

# summary stats -----------------------------------------------------------

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
  
  file <- comp()$table
  
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

# descriptive Statistics

output$stat_summary = renderTable({
  
  comp()$stats
  
})

# assumptions plots -------------------------------------------------------------

output$modelplots <- renderPlot({
  
  p1 <- comp()$diagplot$freq
  p2 <- comp()$diagplot$qqnorm
  p3 <- comp()$diagplot$resid
  p4 <- comp()$diagplot$sresid
  
  ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
  
})

# graphics ----------------------------------------------------------------
# -------------------------------------------------------------------------

observe({
  
  cat("Graphics --------------------------------------------------\n")
  
  cat("input$plot_limit1")
  print(input$plot_limit1)
  
  cat("input$plot_limit2")
  print(input$plot_limit2)
  
  cat("input$plot_ybrakes")
  print(input$plot_ybrakes)
  
  cat("input$stat_fact[1]")
  print(input$stat_fact[1])
  
  cat("input$stat_fact[2]")
  print(input$stat_fact[2])
  
  cat("input$plot_ylab")
  print(input$plot_ylab)
  
  cat("input$plot_xlab")
  print(input$plot_xlab)
  
})

stat_plot <- reactive({

df <- comp()$table

if ( length(input$stat_fact) == 1 ) {
  
  xvar <- input$stat_fact[1]
  gvar <- input$stat_fact[1]
  
} else if ( length(input$stat_fact) == 2 ) {
  
  xvar <- input$stat_fact[1]
  gvar <- input$stat_fact[2]
  
}

if ( !is.na(input$plot_limit1) & !is.na(input$plot_limit2) ) {
  
  limits <- c(input$plot_limit1, input$plot_limit2)
  
} else { limits <- NULL }

if (is.na(input$plot_ybrakes)) { ybrakes <- NULL } else { ybrakes <- input$plot_ybrakes }

if ( input$plot_ylab == "" ) { ylab <- NULL } else { ylab <- input$plot_ylab }
if ( input$plot_xlab == "" ) { xlab <- NULL } else { xlab <- input$plot_xlab }
if ( input$plot_glab == "" ) { glab <- NULL } else { glab <- input$plot_glab }

if (input$plot_sig == "no") { sig <- NULL } else { sig <- input$plot_sig }

# -------------------------------------------------------------------------

   pt <- fplot(data = df
               , type = input$plot_type
               , x = xvar
               , y = input$stat_rsp
               , groups = gvar
               , ylab = ylab
               , xlab = xlab
               , glab = glab
               , legend = input$plot_legend
               , sig = sig
               , error = input$plot_error
               , limits = limits
               , brakes = ybrakes
               , xbrks = NULL
               , gbrks = NULL
               , color = input$plot_color
               )
   
   pt

})

# plot output -------------------------------------------------------------

output$plotgr <- renderImage({
  
  dpi <- input$plot_res
  ancho <- input$plot_width
  alto <- input$plot_height
  
  outfile <- tempfile(fileext = ".png")
  
  png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
  print(stat_plot())
  dev.off()
  
  list(src = outfile)
  
}, deleteFile = TRUE)


# Germination InTime ------------------------------------------------------
# -------------------------------------------------------------------------

output$smvar <- renderUI({
  
  inFile <- fb()

  if (is.null(inFile)) return(NULL)

  evf <- inFile %>% 
    select(!starts_with(input$evalName)) %>% 
    names()

  selectInput('summary_by', 'Summarize by', c(Choose='', evf))
  
})

gnt <- reactive({
  
  inFile <- fb()
  
  if (is.null(inFile)) { return(NULL) }
  
  else if (input$summary_by ==''){ return(NULL) }
  
  else {

    smt <- ger_intime(Factor = input$summary_by
                      , SeedN = input$SeedN
                      , evalName = input$evalName
                      , method = input$intime_type
                      , data = inFile
                      )
  }

})

intime_plot <- reactive({
  
  validate(need( input$summary_by, "Select your response variable" ) )
  
  if ( !is.na(input$intime_limit1) & !is.na(input$intime_limit2) ) {
    
    limits <- c(input$intime_limit1, input$intime_limit2)
    
  } else { limits <- NULL }
  
  if (is.na(input$intime_ybrakes)) { ybrakes <- NULL } else { ybrakes <- input$intime_ybrakes }
  
  if ( input$intime_ylab == "" ) { ylab <- NULL } else { ylab <- input$intime_ylab }
  if ( input$intime_xlab == "" ) { xlab <- NULL } else { xlab <- input$intime_xlab }
  if ( input$intime_glab == "" ) { glab <- NULL } else { glab <- input$intime_glab }
  
  # -------------------------------------------------------------------------
  
  pt <- fplot(data =  gnt()
              , type = "line"
              , x = "evaluation"
              , y = "mean"
              , groups = input$summary_by
              , ylab = ylab
              , xlab = xlab
              , glab = glab
              , legend = input$intime_legend
              , sig = NULL
              , error = input$intime_error
              , limits = limits
              , brakes = ybrakes
              , xbrks = NULL
              , gbrks = NULL
              , color = input$intime_color
  )
  
  pt
  
})


output$intime_plot <- renderImage({
  
  dpi <- input$intime_res
  ancho <- input$intime_width
  alto <- input$intime_height
  
  outfile <- tempfile(fileext = ".png")
  
  png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
  print(intime_plot())
  dev.off()
  
  list(src = outfile)
  
}, deleteFile = TRUE)

# osmotic tools -----------------------------------------------------------
# -------------------------------------------------------------------------

output$ops <- reactive({
  
  if( input$pres > 0) return( "presure should be negative")
  else{  
    
    op <- osmp(type = input$tool_osmp,
         vol = input$vol, 
         pres = input$pres, 
         temp = input$temp, 
         mw = input$psm, 
         ki = input$dis)
    
    paste(op, "g/l", sep = " ")
    
  }
  
}) 


})


# end germinaquant --------------------------------------------------------
# -------------------------------------------------------------------------



