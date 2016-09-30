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
      paste("GerminaQuant-indices-", Sys.Date(), '.csv', sep='')
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
    
    grn <- c("GRS", "GRP", "ASG", "MGT", "MGR", "GSP", "UNC", "SYN", "VGT", "SDG", "CVG")
    
    selectInput('dvar', 'dependent Variable', c(Choose = '', grn))
  })
  
  
  output$out2 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = myData()) # Factor Names
    
    selectInput('ivar1', 'first factor', c(Choose = '', names(evf)))
  })
  
  output$out3 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = myData()) # Factor Names
    
    selectInput('ivar2', 'second factor', c(Choose = '', names(evf)))
  })
  
  output$out4 <- renderUI({
    inFile <- varCal()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = myData()) # Factor Names
    
    selectInput('ivar3', 'block', c(Choose= '', names(evf)))
  })
  


  
# Analisis of Variance ---------------------------------------------------
  
  av <- reactive({
    
    inFile <- varCal()
    
    if (is.null(inFile)){return(NULL)} 
    
    else if(input$ivar1 == '' && input$ivar2 == '' && input$ivar3 == '' && input$dvar == '')
      
    {return(NULL)} 
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$ivar3 == '') && !(input$dvar == '')) 
      
    {
      formula <- as.formula( paste(input$dvar, paste(input$ivar3, paste(input$ivar1, input$ivar2, sep = "*"), sep = " + ") , sep = " ~ ") )
      modelo <- aov(formula, data = inFile)
    }
    
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$dvar == '')) 
      
    {
      formula <- as.formula( paste(input$dvar, paste(input$ivar1, input$ivar2, sep = "*") , sep = " ~ ") )
      modelo <- aov(formula, data = inFile)
    }
    
    else if( !(input$ivar1 == '') && !(input$ivar3 == '') && !(input$dvar == '')) 
      
    {
      formula <- as.formula( paste(input$dvar, paste(input$ivar3, input$ivar1, sep = " + ") , sep = " ~ ") )
      modelo <- aov(formula, data = inFile)
    }
    
    
    else if( !(input$ivar1 == '') && !(input$dvar == '')) 
    
    {
      formula <- as.formula(paste(input$dvar, input$ivar1, sep = " ~ "))
      modelo <- aov(formula, data = inFile)
    }
    
    
  })
  
  
  
  output$tbaov = renderPrint({
    inFile <- av()
    if (is.null(inFile)){ cat("select your variables") }
    else {
      
    summary(inFile)

      
    }
  })
  

  
# Mean Comparation Test ---------------------------------------------------


  MNC <- reactive({
    
    inFile <- av()
    
    if (is.null(inFile)) return(NULL)
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$dvar == '')) 
      
    {
      snk <- agricolae::SNK.test( y = inFile, trt = c(input$ivar1, input$ivar2 ))
      mc <- dtsm(snk)
      mc
    }
    
    
    else if( !(input$ivar1 == '') && !(input$dvar == '')) 
      
    {
      snk <- agricolae::SNK.test( y = inFile, trt = c(input$ivar1))
      mc <- dtsm(snk)
      mc
    }
    
    
  })
  
  
  output$MNC = renderTable({
    
    MNC()
    
  })
  
  
  
  output$downloadmc <- downloadHandler(
    filename = function() {
      paste("GerminaQuant-meancomp-", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      inFile <- MNC()
      write.csv(inFile, file)
    }
  )
  
  

# MultiPlot ---------------------------------------------------------------

  
  output$lbmy <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  output$lbmx <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  
  
  output$lbml <- renderUI({
    actionButton("action", label = input$sample_text)
  })
  

  
  output$barplot = renderPlot({
    
    df <- MNC()
    if (is.null(df)) return(NULL)
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$dvar == '')) 
      
    {
      ggplot(df, aes_string(x = input$ivar1 , y = "mean", fill= input$ivar2))+
        geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2, position=position_dodge(.9)) +
        geom_text(aes(label= sg, y = mean+ste), colour="black", size=3, vjust=-.5, angle = 0, position=position_dodge(.9))+
        scale_y_continuous( input$lbmy ) +
        scale_x_discrete( input$lbmx )+
        scale_fill_hue(input$lbml)+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    else if( !(input$ivar1 == '') && !(input$dvar == '')) 
      
    {
      
      ggplot(df, aes_string(x = input$ivar1 , y = "mean", fill= input$ivar1))+
        geom_bar(position=position_dodge(),colour="black",stat="identity", size=.5)+
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2, position=position_dodge(.9)) +
        geom_text(aes(label= sg, y = mean+ste), colour="black", size=3, vjust=-.5, angle = 0, position=position_dodge(.9))+
        scale_y_continuous( input$lbmy ) +
        scale_x_discrete( input$lbmx )+
        scale_fill_hue(input$lbml)+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    
    
  })
  
  
  output$lineplot = renderPlot({
    
    df <- MNC()
    if (is.null(df)) return(NULL)
    
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$dvar == '')) 
      
    {
      ggplot(df, aes_string(x = input$ivar1, y = "mean", group = input$ivar2, shape= input$ivar2, color= input$ivar2))+
        geom_line()+
        geom_point(size=2)+ 
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2)+
        geom_text(aes(label= sg, y = mean), colour="black", size=3, vjust=-.5, hjust = -.5, angle = 0)+
        scale_color_discrete(input$lbml)+
        scale_shape_discrete(input$lbml)+
        scale_y_continuous(input$lbmy)+
        scale_x_discrete(input$lbmx)+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    else if( !(input$ivar1 == '') && !(input$dvar == '')) 
      
    {
      ggplot(df, aes_string(x = input$ivar1, y = "mean", group = input$ivar1, shape= input$ivar1, color= input$ivar1))+
        geom_line()+
        geom_point(size=2)+ 
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.3, width=.2)+
        geom_text(aes(label= sg, y = mean), colour="black", size=3, vjust=-.5, hjust = -.5, angle = 0)+
        scale_color_discrete(input$lbml)+
        scale_shape_discrete(input$lbml)+
        scale_y_continuous(input$lbmy)+
        scale_x_discrete(input$lbmx)+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    
  })
  
  
  output$boxplot = renderPlot({
    
    df <- varCal()
    
    if (is.null(df)) return(NULL)
    
    else if( !(input$ivar1 == '') && !(input$ivar2 == '') && !(input$dvar == '')) 
      
    {
      ggplot(df, aes_string( x = input$ivar1 , y = input$dvar, fill = input$ivar2))+
        geom_boxplot(outlier.colour = "red", outlier.size = 3)+
        geom_point(position = position_jitterdodge())+
        ylab( input$lbmy )+
        xlab( input$lbmx )+
        scale_fill_discrete( input$lbml )+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    else if( !(input$ivar1 == '') && !(input$dvar == '')) 
      
    {
      ggplot(df, aes_string( x = input$ivar1 , y = input$dvar, fill = input$ivar1))+
        geom_boxplot(outlier.colour = "red", outlier.size = 3)+
        geom_point(position = position_jitterdodge())+
        ylab( input$lbmy )+
        xlab( input$lbmx )+
        scale_fill_discrete( input$lbml )+
        theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
    }
    
    
    
  })
  
  
  
# Germination InTime ------------------------------------------------------

  
  output$smvar <- renderUI({
    inFile <- myData()
    if (is.null(inFile)) return(NULL)
    
    evf <- evalFactor(evalName = input$evalName , data = inFile)
    
    selectInput('smvar', 'summarize variable', c(Choose='', names(evf)))
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
      
    ggplot2::ggplot(df, aes_string(df$evaluation, df$mean, group = input$smvar, color = input$smvar, shape = input$smvar)) +
      geom_line() +
      geom_point(size=2)+
      theme_bw()+
      ylab("Germination (%)") +
      xlab(input$lgnt)+
      theme_bw()+
        theme(
          axis.title.x = element_text(face="bold", size=15),
          axis.title.y = element_text(face="bold", size=15, angle=90),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size=12), 
          legend.text = element_text(size=11),
          legend.key.size = unit(1.2, "lines"),
          legend.key = element_blank()
        )
      
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
     
ggplot2::ggplot(df, aes_string(df$evaluation, df$mean, group = input$smvar, color = input$smvar, shape = input$smvar)) +
       geom_line() +
       geom_point(size=2)+
       theme_bw()+
       ylab("Relative Germination") +
       xlab(input$lgnt)+
       theme_bw()+
       theme(
         axis.title.x = element_text(face="bold", size=15),
         axis.title.y = element_text(face="bold", size=15, angle=90),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         legend.title = element_text(face="bold", size=12), 
         legend.text = element_text(size=11),
         legend.key.size = unit(1.2, "lines"),
         legend.key = element_blank()
       )
   }
 })  



 
})
