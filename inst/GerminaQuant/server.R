# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-04-20
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/GerminaR")

source("pkgs.R")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
# close auto local session ------------------------------------------------

observe({
  
  if(Sys.getenv('SHINY_PORT') == "") {
    
    session$onSessionEnded(stopApp)
    
  }
  
})
  
# -------------------------------------------------------------------------
  
  data_fb <-  eventReactive(input$reload, {
    
    if ( !is.null(input$import_excel) ) {
      
      dt <-  readxl::read_excel(path = input$import_excel$datapath
                                , sheet = input$sheetdt) %>% 
        as.data.frame()
      
    } else if ( input$import_gsheet != "" ){
      
      dt <- gsheet::gsheet2tbl(url = input$import_gsheet) %>% 
        as.data.frame()
      
    } else { return(NULL) }
    
  }, ignoreNULL = FALSE)


# data viewer -------------------------------------------------------------
# -------------------------------------------------------------------------
  
output$fb_excel <- DT::renderDataTable(server = FALSE, {
  
  validate( need( is.data.frame(data_fb())
                  , "Insert a Google spreadsheet URL or xlsx file") )
  
  webTable(data = data_fb()
           , file_name = "FieldBook")
  
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

# index calculation --------------------------------------------------------

varCal <- reactive({
  
  validate( need( data_fb(), "Insert a Google spreadsheet URL or xlsx file") )
  
  data_fb() %>% 
    ger_summary(SeedN = input$SeedN
              , evalName = input$evalName
              , data = . )
  
})

output$summary <- DT::renderDataTable(server = FALSE, {
  
  webTable(data = varCal()
           , file_name = "GerminaQuant-indices")
  
})

# boxplot -----------------------------------------------------------------

var_names <- reactive({
  
 vars <- c("grs"
           , "grp"
           , "mgt"
           , "mgr"
           , "gsp"
           , "unc"
           , "syn"
           , "vgt"
           , "sdg"
           , "cvg")
  
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

# boxplot -----------------------------------------------------------------

boxplot <- reactive({
  
  validate(
    need( input$ybp, "Select your response variable")
    , need( input$xbp, "Select your X axis variable" )
    )
  
  ylimits <- input$bpbrk %>% 
    strsplit(split = "[*]") %>% 
    unlist() %>% 
    as.numeric()
  
  xrot <- input$bprot %>% 
    strsplit(split = "[*]") %>% 
    unlist() %>% 
    as.numeric()

  varCal() %>% 
    ger_boxp(y = input$ybp
             , x = input$xbp
             , group = if(input$zbp == "") NULL else input$zbp
             , xlab = if(input$bplx == "") NULL else input$bplx
             , ylab = if(input$bply == "") NULL else input$bply
             , glab = if(input$bplz == "") NULL else input$bplz
             , ylimits =  if(input$bpbrk == "") NULL else ylimits
             , xrotation = if(input$bprot == "") NULL else xrot
             , opt = if(input$bpopt == "") NULL else input$bpopt
             , legend = input$bplg
             )

})

output$boxplot <- renderImage({
  
  dpi <- input$bprs
  ancho <- input$bpwd
  alto <- input$bphg
  
  outfile <- tempfile(fileext = ".png")
  
  png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
  print(boxplot())
  dev.off()
  
  list(src = outfile)
  
}, deleteFile = TRUE)


# statistics --------------------------------------------------------------

# Select factors

output$stat_response <- renderUI({

  fbn <- varCal() %>% 
    select(var_names()) %>% 
    names() 

  selectInput(
    inputId = "stat_rsp",
    label = "Response",
    choices = c("choose" = "", fbn)
  )

})


output$stat_factor <- renderUI({

  fbn <- varCal() %>% 
    select(!var_names()) %>%
    names() 

  selectInput(
    inputId = "stat_fact",
    label = "Factors",
    choices = c("choose" = "", fbn),
    multiple = TRUE
  )

})



output$stat_block <- renderUI({

  fbn <- varCal() %>% 
    select(!var_names()) %>%
    names() 
  
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

    factor <- input$stat_fact %>% 
      paste0() %>%  
      paste(collapse= " * ")

    block <- input$stat_blk %>% 
      paste0() %>% 
      paste(collapse= " + ")
    
    file <- file %>% 
      dplyr::mutate(across(c(input$stat_fact, input$stat_blk), as.factor))
    
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

output$mnc <-  DT::renderDataTable(server = FALSE, {
  
  file <- comp()$table
  
  webTable(data = file
           , file_name = input$stat_rsp)
  
})

# descriptive Statistics

output$stat_summary = renderTable({
  
  comp()$stats
  
})

# assumptions plots -------------------------------------------------------------

output$modelplots <- renderPlot({
  
  diag <- comp()$diagplot 
  plot_grid(plotlist = diag, ncol = 2)
  
})

# graphics ----------------------------------------------------------------
# -------------------------------------------------------------------------

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
  
  inFile <- data_fb()

  if (is.null(inFile)) return(NULL)

  evf <- inFile %>% 
    select(!starts_with(input$evalName)) %>% 
    names()

  selectInput('summary_by', 'Summarize by', c(Choose='', evf))
  
})

gnt <- reactive({
  
  inFile <- data_fb()
  
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

