library(shiny)
library(ggplot2)
library(GerminaR)


shinyServer(function(input, output){
  

# Intro -------------------------------------------------------------------

var <- data.frame(
  
  "variables" = c("germinated seed number", "germination", "germination asin", "mean germination time", "germination speed", "mean germination rate", "germination synchrony", "germination uncertany", "germination standard deviation", "germination variance", "coef. variance of germinaton"),
  "abbreviation" = c("GRS", "GRP", "ASG", "MGT", "GSP", "MGR",  "SYN", "UNC",  "SDG","VGT", "CVG"),
  "limits" = c("\\(0 \\le n \\le n_i\\)", "\\(0 \\le g \\le 100\\)", "\\(0 \\le arsin \\le 1\\)", "\\(0 \\le t \\le k\\)", "\\(0 < g_s \\le 100\\)", "\\(0 < v \\le 1\\)", "\\(0 \\le Z \\le 1\\)", "\\(0 \\le U \\le log_2 n_i\\)", "\\(0 < s_t^2  \\le \\infty\\)", "\\(0 < s_t  \\le \\infty\\)", "\\(0 < CV_t  \\le \\infty\\)"),
  "units" = c("\\(count\\)", "\\(\\%\\)", "\\(grade\\)", "\\(time\\)", "\\(\\%\\)", "\\(time^{-1}\\)", "\\(-\\)", "\\(bit\\)", "\\(time^2\\)", "\\(time\\)", "\\(\\%\\)")
)


output$var <- renderTable({
  
  var
  
})



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
  
  
  output$summary <- renderDataTable({
    
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


# osmotic tools -----------------------------------------------------------


output$ops <- reactive({

  if( input$pre > 0) return( "presure should be negative")
  else{  
     
r1 <- (-0.00820574587 * (input$tem + 273) * input$dis)

r2 <- input$pre/r1

r3 <- r2 * 1000

r4 <- (input$psm * r3 * input$vol*1000)/10^6

round(r4,4)

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

}

}) 
 

 
# GermBook ----------------------------------------------------------------
 
 
 output$usmn <- renderUI({

   gb <- tags$iframe(src = "https://bookdown.org/flavjack/germinaquant/", style="height:600px; width:100%; scrolling=yes")

   print(gb)

 })
 
 
 output$dtsm <- renderUI({

   gb <- tags$iframe(src = "https://docs.google.com/spreadsheets/d/1QziIXGOwb8cl3GaARJq6Ez6aU7vND_UHKJnFcAKx0VI/edit?usp=sharing", style="height:600px; width:100%; scrolling=yes")

   print(gb)

 }) 
 
})