# -------------------------------------------------------------------------
# GerminaR-----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/GerminaR/
#> open https://flavjack.shinyapps.io/germinaquant/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-10-10
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
    
    fb <- if ( !is.null(input$import_excel) ) {
      
      dt <-  readxl::read_excel(path = input$import_excel$datapath
                                , sheet = input$sheetdt) %>% 
        as.data.frame()
      
    } else if ( input$import_gsheet != "" ){
      
      dt <- gsheet::gsheet2tbl(url = input$import_gsheet) %>% 
        as.data.frame()
      
    } else { return(NULL) }
    
    fb %>% 
      select(!starts_with("[") | !ends_with("]")) 
    
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
    label = "Block (RCBD)",
    choices = c("choose" = "", fbn),
    multiple = FALSE
  )

})

output$stat_comparison <- renderUI({
  
  factors <- if(input$stat_model == "auto") { 
    
    input$stat_fact
    
  } else if (input$stat_model == "manual") {
    
    input$stat_model_factors %>% 
      strsplit(.,'[[:punct:]]+') %>% 
      pluck(1) %>% 
      base::trimws()
    
  }
  
  selectInput(
    inputId = "stat_comparison",
    label = "Comparison",
    choices = c("choose" = "", factors),
    multiple = TRUE
  )
  
})


# germination indices -----------------------------------------------------

analysis <- reactive({
  
  validate(need(input$stat_rsp, "Choose your response variable"))
  validate(need(input$stat_comparison, "Choose your factors to compare"))
  
  factors <- if(input$stat_model == "auto") { 
    
    input$stat_fact
    
    } else if (input$stat_model == "manual") {
      
    input$stat_model_factors
    
    }
  
  bloque <- if(input$stat_model == "auto") { 
    
    input$stat_blk
    
  } else if (input$stat_model == "manual") { NA }
  
  validate(need(factors, "Choose your model factors"))
  
  gquant_analysis(data = varCal()
                  , response = input$stat_rsp
                  , factors = factors
                  , block = bloque
                  , comparison = input$stat_comparison
                  , type = input$stmc
                  , sig = input$stsig
                  )
  })

# anova -------------------------------------------------------------------

output$tbav  <- renderPrint({ summary(analysis()$aov ) })

output$model_formula <- renderPrint({ analysis()$param$formula })

output$graph_formula <- renderPrint({ analysis()$param$formula })

# comparison table --------------------------------------------------------

output$mnc <-  DT::renderDataTable(server = FALSE, {
  
  webTable(data = analysis()$data$table
           , file_name = input$stat_rsp)
  })

# descriptive Statistics

output$stat_summary <-  DT::renderDataTable(server = FALSE, {
  
  webTable(data = analysis()$data$stat
           , scrolly = "12vh"
           , buttons = "copy")
  
})

# assumptions plots -------------------------------------------------------------

output$modelplots <- renderPlot({
  
  diag <- analysis()$data$diagplot 
  plot_grid(plotlist = diag, ncol = 2)
  
})

# graphics ----------------------------------------------------------------
# -------------------------------------------------------------------------

stat_plot <- reactive({
  
  validate(need(analysis(), "Choose your model parameters"))

if ( length(input$stat_comparison) == 1 ) {
  
  xvar <- input$stat_comparison[1]
  gvar <- input$stat_comparison[1]
  
} else if ( length(input$stat_comparison) == 2 ) {
  
  xvar <- input$stat_comparison[1]
  gvar <- input$stat_comparison[2]
  
}

fplot(data = analysis()$data$table
      , type = input$plot_type
      , y = input$stat_rsp
      , x = xvar
      , group = gvar
      , ylab = input$plot_ylab
      , xlab = input$plot_xlab
      , glab = input$plot_glab
      , legend = input$plot_legend
      , sig = input$plot_sig 
      , error = input$plot_error
      , color = input$plot_color
      , ylimits = input$plot_ylimits
      , xtext = input$plot_xbrakes
      , gtext = input$plot_gbrakes
      , xrotation = input$plot_xrotation
      , opt = input$plot_opt
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
  
  fplot(data = gnt()
        , type = "line"
        , x = "evaluation" 
        , y = "mean"
        , group = input$summary_by
        , ylab = input$intime_ylab
        , xlab = input$intime_xlab
        , glab = input$intime_glab
        , legend = input$intime_legend
        , color = input$intime_color
        , ylimits = input$intime_ylimits
        , xrotation = input$intime_xrotation
        # , xtext = input$intime_xbrakes
        , gtext = input$intime_gbrakes
        , error = input$intime_error
        , opt =  input$intime_opt
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

