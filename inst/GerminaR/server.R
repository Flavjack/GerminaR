library(shiny)
library(GerminaR)
library(agricolae)
library(ggplot2)


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
    ger_summary(inFile )
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

  mc <- reactive({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    cp <- SNK.test( y = inFile, trt = input$ivar)
    
    sm <- mutate(cp$means, trt = row.names(cp$means), ste = std/sqrt(r))
    sm <- full_join(sm, cp$groups)
    sm <- select(sm,trt, means, std, r, ste, M)
    
    list(cp$statistics, sm)
  })
  
  
  output$mcp = renderPrint({
    
    mc()
    
  })
    
    
# Plot --------------------------------------------------------------------
  
  dt <- reactive({
    inFile <- mc()
    if (is.null(inFile)) return(NULL)
    df <- as.data.frame(inFile[[2]])
  
    })
  
  
  output$Barplot = renderPlot({
    
    df <- dt()
    if (is.null(df)) return(NULL)
    ggplot(df, aes(y =  means , x =  trt , fill = trt))+
      geom_bar(stat = "identity")+
      geom_errorbar(aes(ymin= means - ste , ymax= means + ste), size=.3,width=.2)+
      ylab( " " )+
      xlab(" ")+
      theme_bw()
    
  })
  
  
  output$x <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('x', 'Eje X', c(Choose='', names(inFile)))
  })
  
  output$y <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('y', 'Eje Y', c(Choose='', names(inFile)))
  })
  
  output$g <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    selectInput('group', 'Grouped by', c(Choose='', names(inFile)))
  })
  
  
  output$Boxplot = renderPlot({
    
    df <- varCal()
    if (is.null(df)) return(NULL)
    ggplot(df, aes_string( input$x , input$y ))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
      ylab( " " )+
      xlab(" ")+
      theme_bw()
    
  })
  
    
})

# df <- varCal()
# if (is.null(df)) return(NULL)
# ggplot(df, aes( input$x , input$y, fill = input$g ))+
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
#   ylab( " " )+
#   xlab(" ")+
#   theme_bw()
