library(shiny)
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
  
  
  output$SeedN <- renderPrint({ input$text })
  
  output$evalName <- renderPrint({ input$text })
  
  output$contents <- renderTable({
    
    myData()
    
  })
  
  
  # Index Caculation --------------------------------------------------------
  
  
  varCal <- reactive({
    inFile <- myData()
    if (is.null(inFile )) return(NULL)
    GerminaR::ger_summary(SeedN = input$SeedN , evalName = input$evalName , data = inFile  )
  })
  
  
  output$summary <- renderTable({
    
    varCal()
    
  })
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("GerminaQuant-", Sys.Date(), '.csv', sep='')
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
    
    evf <- evalFactor(evalName = input$evalName , data = myData()) # Factor Names
    
    selectInput('ivar', 'Independent Variable', c(Choose='', names(evf)))
  })
  
  
  output$out2 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    grn <- c("GRS", "GRP", "ASG", "MGT", "MGR", "GSP", "UNC", "SYN", "VGT", "SDG", "CVG")
    
    selectInput('dvar', 'Dependent Variable', c(Choose='', grn))
  })
  
  
  # Analisis of Variance ---------------------------------------------------
  
  av <- reactive({
    inFile <- varCal()
    if (is.null(inFile)){return(NULL)
    } else if(input$ivar ==''|| input$dvar == ''){
      return(NULL)
    } else {
      
      formula <- as.formula(paste(input$dvar, paste(input$ivar, collapse=" + "), sep=" ~ "))
      modelo <- aov(formula, data = inFile)
      modelo
      
    }
  })
  
  
  output$aovSummary = renderPrint({
    inFile <- av()
    if (is.null(inFile)){ cat("Please select your variables") }
    else {
      
      summary(inFile)
      
    }
  })
  


# Mean Comparation Test ---------------------------------------------------


  MNC <- reactive({
    inFile <- av()
    if (is.null(inFile)) return(NULL)
    
    hsd <- agricolae::HSD.test( y = inFile, trt = input$ivar)
    snk <- agricolae::SNK.test( y = inFile, trt = input$ivar)
    dnc <- agricolae::duncan.test( y = inFile, trt = input$ivar)
    
    sm <- dplyr::mutate(snk$means, trt = row.names(snk$means), means = snk$means[ , 1] , ste = std/sqrt(r))
    
    sm <- dplyr::select(sm, trt, means, std, r, ste)
    
    list(
      Statistics = snk$statistics, 
      Summary = sm,  
      Tukey = hsd$groups , 
      Student_Newman_Keuls = snk$groups, 
      Duncan = dnc$groups
      )
    
  })
  
  
  output$MNC = renderPrint({
    
    MNC()
    
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
    inFile <- MNC()
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
  


# Germination InTime ------------------------------------------------------

  
  output$smvar <- renderUI({
    inFile <- myData()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = inFile)
    
    selectInput('smvar', 'Summarize Variable', c(Choose='', names(evf)))
  })
  

 
 gntp <- reactive({
    inFile <- myData()
    if (is.null(inFile)) { return(NULL) }
    else if (input$smvar ==''){ return(NULL) }
    else {

    smt <- GerminaR::ger_intime( input$smvar, input$SeedN , input$evalName, "percentage", inFile)

    }

 })  
 

 output$gertimep <- renderTable({
  
   gntp()
   
 })
  
 
 
 output$lgnt <- renderUI({
   actionButton("action", label = input$sample_text)
 })
 
 
 output$GerInTimep = renderPlot({
    df <- gntp()
    if (is.null(df)) return(NULL)
    else if (input$smvar =='' ){ return(NULL) }
    else{
      
    ggplot2::ggplot(df, aes_string(df$variable, df$value, group = input$smvar, color = input$smvar)) +
      geom_line() +
      geom_point(shape=19, size=2)+
      theme_bw()+
      ylab("Germination (%)") +
      xlab(input$lgnt)+
      theme_bw()
      
    }
  })  
 

 
 gntr <- reactive({
   inFile <- myData()
   if (is.null(inFile)) { return(NULL) }
   else if (input$smvar ==''){ return(NULL) }
   else {
     
     smt <- GerminaR::ger_intime( input$smvar, input$SeedN, input$evalName, "relative", inFile)
     
   }
   
 })  

 
 output$gertimer <- renderTable({
   
   gntr()
   
 })
 

 output$GerInTimer = renderPlot({
   df <- gntr()
   if (is.null(df)) return(NULL)
   else if (input$smvar =='' ){ return(NULL) }
   else{
     
     ggplot2::ggplot(df, aes_string(df$variable, df$value, group = input$smvar, color = input$smvar)) +
       geom_line() +
       geom_point(shape=19, size=2)+
       theme_bw()+
       ylab("Relative Germination") +
       xlab(input$lgnt)+
       theme_bw()
     
   }
 })  

  
 
 output$gertimer <- renderTable({
   
   gntr()
   
 })
 

 output$GerInTimer = renderPlot({
   df <- gntr()
   if (is.null(df)) return(NULL)
   else if (input$smvar =='' ){ return(NULL) }
   else{
     
     ggplot2::ggplot(df, aes_string(df$variable, df$value, group = input$smvar, color = input$smvar)) +
       geom_line() +
       geom_point(shape=19, size=2)+
       theme_bw()+
       ylab("Relative Germination") +
       xlab(input$lgnt)+
       theme_bw()
     
   }
 })  
 
  
 
# MultiPlot ---------------------------------------------------------------


 output$ex <- renderUI({
   inFile <- varCal()
   if (is.null(inFile)) return(NULL)
   
   evf <- evalFactor(evalName = input$evalName , data = myData())
   
   selectInput('ex', 'Axis X', c(Choose='', names(evf)))
 })
 
  output$ey <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    grn <- c("GRS", "GRP", "ASG", "MGT", "MGR", "CVL", "GRU", "GSI", "VGT", "SDG", "CVG")
    
    selectInput('ey', 'Axis Y', c(Choose='', grn))
  })
  
  output$eg <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = myData())
    
    selectInput('eg', 'Grouped', c(Choose='', names(evf)))
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
  
  
  
  output$Boxplot = renderPlot({
    df <- varCal()
    if (is.null(df)) return(NULL)
    else if (input$ex=='' || input$ey=='' || input$eg ==''){ return(NULL)}
    else{
    ggplot2::ggplot(df, aes_string( input$ex , input$ey, fill = input$eg ))+
      ggplot2::geom_boxplot(outlier.colour = "red", outlier.size = 2)+
      ylab( input$lby )+
      xlab( input$lbx )+
      scale_fill_discrete( input$lbg )+
      theme_bw()
    }
  })
  
  output$Dotplot = renderPlot({
    df <- varCal()
    if (is.null(df)) return(NULL)
    else if (input$ex=='' || input$ey=='' || input$eg ==''){ return(NULL)}
    else{
    ggplot2::ggplot(df, aes_string( input$ex , input$ey, color = input$eg))+
      geom_point(size = 2)+
      ylab( input$lby )+
      xlab( input$lbx )+
      scale_color_discrete( name = input$lbg ) +
      theme_bw()
    }
  })
  
})
