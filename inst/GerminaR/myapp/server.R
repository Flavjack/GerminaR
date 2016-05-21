library(shiny)
library(dplyr)
library(agricolae)
library(ggplot2)
library(GerminaR)

shinyServer(function(input, output) {
  
# Import Data -------------------------------------------------------------

  myData <- reactive({
    inFile <- input$data
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  
  output$contents <- renderTable({
    
    myData()
    
  })
  
# Index Caculation --------------------------------------------------------
  
  
  varCal <- reactive({
    inFile <- myData()
    if (is.null(inFile )) return(NULL)
    ger_summary(inFile, evalName = "D"  )
  })
  
  
  output$summary <- renderTable({
    
    varCal()
    
  })
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GerminaR-", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      inFile <- varCal()
      write.csv(inFile, file)
    }
  )
  

# Choose Variables ---------------------------------------------------
  
  output$out1 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('ivar', 'Independent Variable', c(Choose='', names(inFile)))
    })
  
  output$out2 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('dvar', 'Dependent Variable', c(Choose='', names(inFile)))
  })
  
  
# Analisis of Variance ---------------------------------------------------

  av <- reactive({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    formula <- as.formula(paste(input$dvar, paste(input$ivar, collapse=" + "), sep=" ~ "))
    aov(formula, data=inFile)
    
  })
  
  
  output$aovSummary = renderPrint({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    summary(inFile)
    
  })
  


# Mean Comparation Test ---------------------------------------------------

  HSD <- reactive({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    cp <- agricolae::HSD.test( y = inFile, trt = input$ivar)
    
    sm <- mutate(cp$means, trt = row.names(cp$means), ste = std/sqrt(r))
    sm <- full_join(sm, cp$groups)
    sm <- select(sm,trt, means, std, r, ste, M)
    
    list(cp$statistics, sm)
  })
  
  
  output$HSD = renderPrint({
    
    HSD()
    
  })
  
  
  
  SNK <- reactive({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    cp <- SNK.test( y = inFile, trt = input$ivar)
    
    sm <- mutate(cp$means, trt = row.names(cp$means), ste = std/sqrt(r))
    sm <- full_join(sm, cp$groups)
    sm <- select(sm,trt, means, std, r, ste, M)
    
    list(cp$statistics, sm)
  })
  
  
  output$SNK = renderPrint({
    
    SNK()
    
  })
    
  DNC <- reactive({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    cp <- duncan.test( y = inFile, trt = input$ivar)
    
    sm <- mutate(cp$means, trt = row.names(cp$means), ste = std/sqrt(r))
    sm <- full_join(sm, cp$groups)
    sm <- select(sm,trt, means, std, r, ste, M)
    
    list(cp$statistics, sm)
  })
  
  
  output$DNC = renderPrint({
    
    DNC()
    
  })
  
# Mean Plot --------------------------------------------------------------------
  
  output$lbmy <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  output$lbmx <- renderUI({
    actionButton("action", label = input$sample_text)
  })

  
  output$lbml <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  
  dt <- reactive({
    inFile <- SNK()
    if (is.null(inFile)) return(NULL)
    df <- as.data.frame(inFile[[2]])
  
    })
  
  
  output$Barplot = renderPlot({
    
    df <- dt()
    if (is.null(df)) return(NULL)
    ggplot2::ggplot(df, aes(y =  means , x =  trt , fill = trt))+
      geom_bar(stat = "identity")+
      geom_errorbar(aes(ymin= means - ste , ymax= means + ste), size=.3,width=.2)+
      ylab( input$lbmy )+
      xlab(input$lbmx)+
      scale_fill_hue(name= input$lbml )+
      theme_bw()
    
  })
  


# Multi Plot --------------------------------------------------------------


  output$ex <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('ex', 'Eje X', c(Choose='', names(inFile)))
  })
  
  output$ey <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('ey', 'Eje Y', c(Choose='', names(inFile)))
  })
  
  
  output$lbx <- renderUI({
    actionButton("action", label = input$sample_text)
  })

  output$lby <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  output$lbg <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  
  
  output$eg <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('eg', 'Grouped', c(Choose='', names(inFile)))
  })
  
  
  output$Boxplot = renderPlot({
    df <- varCal()
    if (is.null(df)) return(NULL)
    ggplot2::ggplot(df, aes_string( input$ex , input$ey, fill = input$eg ))+
      ggplot2::geom_boxplot(outlier.colour = "red", outlier.size = 2)+
      ylab( input$lby )+
      xlab( input$lbx )+
      scale_fill_discrete( input$lbg )+
      theme_bw()
      
      
      
  })
  
  output$Dotplot = renderPlot({
    df <- varCal()
    if (is.null(df)) return(NULL)
    ggplot2::ggplot(df, aes_string( input$ex , input$ey, color = input$eg))+
      geom_point(size = 2)+
      ylab( input$lby )+
      xlab( input$lbx )+
      scale_color_discrete( name = input$lbg ) +
      theme_bw()
      
  })
  
})
