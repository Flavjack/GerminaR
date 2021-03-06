# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-04-29
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/GerminaR")

suppressPackageStartupMessages({source("pkgs.R")})

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
           , "cvg"
           )
  
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
  
  validate(need(boxplot(), "Choose your variables"))
  
  
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

output$stat_summary <-  DT::renderDataTable(server = FALSE, {
  
  webTable(data = comp()$stats
           , scrolly = "12vh"
           , buttons = "copy")
  
})

# assumptions plots -------------------------------------------------------------

output$modelplots <- renderPlot({
  
  diag <- comp()$diagplot 
  plot_grid(plotlist = diag, ncol = 2)
  
})

# graphics ----------------------------------------------------------------
# -------------------------------------------------------------------------

stat_plot <- reactive({
  
  validate(need(input$stat_fact, "Choose your factors and response variable"))

if ( length(input$stat_fact) == 1 ) {
  
  xvar <- input$stat_fact[1]
  gvar <- input$stat_fact[1]
  
} else if ( length(input$stat_fact) == 2 ) {
  
  xvar <- input$stat_fact[1]
  gvar <- input$stat_fact[2]
  
}

ylimits <- input$plot_ylimits %>% 
  strsplit(split = "[*]") %>% 
  unlist() %>% 
  as.numeric()

plot_xrotation <- input$plot_xrotation %>% 
  strsplit(split = "[*]") %>% 
  unlist() %>% 
  as.numeric()

xtext <- input$plot_xbrakes %>% 
  strsplit(split = ",") %>% 
  unlist()

gtext <- input$plot_gbrakes %>% 
  strsplit(split = ",") %>% 
  unlist() 

# -------------------------------------------------------------------------

fplot(data = comp()$table
      , type = input$plot_type
     , x = xvar
     , y = input$stat_rsp
     , group = gvar
     , ylab = if (input$plot_ylab == "") NULL else input$plot_ylab
     , xlab = if (input$plot_xlab == "") NULL else input$plot_xlab
     , glab = if (input$plot_glab == "") NULL else input$plot_glab
     , legend = input$plot_legend
     , sig = if(input$plot_sig == "no") NULL else input$plot_sig
     , error = if(input$plot_error == "no") NULL else input$plot_error
     , color = if(input$plot_color == "yes") TRUE else FALSE
     , ylimits = if(input$plot_ylimits == "") NULL else ylimits
     , xtext = if(input$plot_xbrakes == "") NULL else xtext
     , gtext = if(input$plot_gbrakes == "") NULL else gtext
     , xrotation = if(input$plot_xrotation == "") NULL else plot_xrotation
     , opt = if(input$plot_opt == "") NULL else input$plot_opt
     )

})

# plot output -------------------------------------------------------------

output$plotgr <- renderImage({
  
  validate(need(stat_plot(), "Choose your model"))
  
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
  
  validate(need(input$summary_by, "Choose your factor"))
  
  inFile <- data_fb()
  
  if (is.null(inFile)) { return(NULL) }
  
  else if (input$summary_by ==''){ return(NULL) }
  
  else {
    
    ts <- ger_intime(Factor = input$summary_by
               , SeedN = input$SeedN
               , evalName = input$evalName
               , method = input$intime_type
               , data = inFile
               )
  }

})

intime_plot <- reactive({
  
  validate(need( gnt(), "Select your response variable" ) )
  
  ylimits <- input$intime_ylimits %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

  plot_xrotation <- input$intime_xrotation %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

  xtext <- input$intime_xbrakes %>%
    strsplit(split = ",") %>%
    unlist()

  gtext <- input$intime_gbrakes %>%
    strsplit(split = ",") %>%
    unlist()

  # -------------------------------------------------------------------------
  
  fplot(data = gnt()
        , type = "line"
        , x = "evaluation" 
        , y = "mean"
        , group = input$summary_by
        , ylab = if (input$intime_ylab == "") NULL else input$intime_ylab
        , xlab = if (input$intime_xlab == "") NULL else input$intime_xlab
        , glab = if (input$intime_glab == "") NULL else input$intime_glab
        , legend = input$intime_legend
        , color = if(input$intime_color == "yes") TRUE else FALSE
        , ylimits = if(input$intime_ylimits == "") NULL else ylimits
        , xrotation = if(input$intime_xrotation == "") NULL else plot_xrotation
        , xtext = if(input$intime_xbrakes == "") NULL else xtext
        , gtext = if(input$intime_gbrakes == "") NULL else gtext
        , error = if(input$intime_error == "no") NULL else input$intime_error
        , opt = if(input$intime_opt == "") NULL else input$intime_opt
        )
  
})


output$intime_plot <- renderImage({
  
  validate(need(intime_plot(), "Select your factor"))
  
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

