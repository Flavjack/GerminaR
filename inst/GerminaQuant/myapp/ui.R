library(shiny)
library(ggplot2)
library(GerminaR)


shinyUI(navbarPage("GerminaQuant",
                   
# Introduction ------------------------------------------------------------
                   
                   
                   tabPanel("Introduction",icon = icon("home", lib = "glyphicon"), 
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                img(src = "germinaquant.png", height = 200, width = 200),
                                span(
                                  p(a("Plant Physiology Laboratory (UFPE)", 
                                      href = "https://www.ufpe.br/lev/", 
                                      target="_blank")), 
                                  style = "color:black"),
                                
                                p("GerminaQuant is web application based in R, you can use the app in your desktop installing the package", em("GerminaR")),
                                code('install.packages(GerminaR)'),
                                br(),
                                br(),
                                p("For use the interactive app:"),
                                code("library(GerminaR)"),
                                br(),
                                code("runGerminaQuant()"),
                                br(),
                                br(),
                                
                                
                                p(
                                  a("User Manual", 
                                    href = "https://bookdown.org/flavjack/germinaquant/", target="_blank")
                                ),
                                
                                p(
                                a("Data Sample", 
                                    href = "https://docs.google.com/spreadsheets/d/1QziIXGOwb8cl3GaARJq6Ez6aU7vND_UHKJnFcAKx0VI/edit?usp=sharing", target="_blank")
                                ),
                                
                                br(),
                                
                                strong("Authors"),
                                br(),
                                div("Lozano-Isla, Flavio ", style = "color:green"),
                                div("Benites-Alfaro, Omar ", style = "color:green"),
                                div("Pompelli, Marcelo F.", style = "color:green")
                                
                            
                                
                              ),
                              
                              mainPanel(
                                
                                
                              shiny::HTML("<p align='justify'>GerminaQuant allows make the calculation and graph  of the germination variables <i>incredibly easy</i> 
                                            in an interactive applications. GerminaQuant is reactive!, outputs change instantly as users modify inputs, without requiring a reload the app.</p>"),

                                shiny::HTML("<h4><b>Features</b></h4>"),
                                
                                shiny::HTML("<p>
                                            <ol>
                                            <li>Allow calculate the princiapal germination variables.</li>
                                            <li>Statistical analysis for germination variables.</li>
                                            <li>Easy way to plot the results.</li>
                                            </ol>
                                            </p>"),
                                hr(),
                                br(),
                                
                                
                                shiny::HTML("<h4><b>Evaluation of seed germination process</b></h4>"),
                                
                       
                                
                              shiny::HTML("<p align='justify'> The physiology and seed technology have provided valuable tools for the production of high quality seed and treatments and storage conditions (MARCOS-FILHO, 1998). 
                                          In basic research, the seeds are studied exhaustively, and the approach of its biology is performed to fully exploit the dormancy and germination (PENFIELD; KING, 2009). 
                                          An important tool to indicate the performance of a seed lot is the precise quantification of germination through accurate analysis of the cumulative germination data (JOOSEN et al., 2010).
                                          Time, speed, homogeneity and synchrony are aspects that can be measured, and inform the dynamics of the germination process. 
                                          These characteristics are interesting not only for physiologists and seed technologists, but also for ecologist, since it is possible to predict the degree of success of the species, 
                                          based on the seed crop ability to redistribute germination over time, allowing the recruitment the part of the environment formed seedlings (RANAL; SANTANA, 2006).   </p>"),
                              
                             
                              shiny::HTML("<h4><b>Germination (g)</b></h4>"),
                              
                              
                             
                              shiny::HTML("<p align='justify'> According GOUVEA LABOURIAU (1983), the germinability of a sample of is the percentage of seeds in which the seed germination process comes to the end, 
                                          in experimental conditions by the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo. In general, it is presented as percentage, 
                                          accompanied by some degree of dispersion, but it is possible to use proportions to one or more samples may be subjected to statistical tests (CARVALHO; SANTANA, 2005)</p>"),

                                
                                img(src = "GRM.png", height = 60, width = 120),
                                br(),
                                
                              shiny::HTML("<h4><b>Mean germination time (t)</b></h4>"),
                              
                              
                              shiny::HTML("<p align='justify'>It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. 
                                          The number of germinated seeds at the intervals established for the collection of data is used as weight. 
                                          It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962)</p>"),          

  
                              
                                img(src = "MGT.png", height = 60, width = 120),
                                br(),
                              
                                   
                              shiny::HTML("<h4><b>Mean germination rate (v)</b></h4>"),
                              
                              
                                shiny::HTML("<p align='justify'>The average speed of germination is defined as the reciprocal of the average 
                                              time germination (RANAL; SANTANA, 2006)</p>"),  
   
                              
                                img(src = "MGR.png", height = 50, width = 80),
                                br(),
                                
                              shiny::HTML("<h4><b>Uncertainty index (u)</b></h4>"),
                              
                           
                              shiny::HTML("<p align='justify'> The uncertainty index (u) is an adaptation of Shannon index measures the 
                                            degree of uncertainty in predicting the informational entropy or uncertainty associated with the 
                                            distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). 
                                  Low values of u indicate frequencies with short peaks, i.e. the more concentrated the germination in time. 
                                          Just a germinated seed changes the value of u. This means that u measures the degree of germination scattering.</p>"),

                                 
                                img(src = "UCI.png", height = 60, width = 240),
                                br(),
                                
                             shiny::HTML("<h4><b>Synchrony index (Z)</b></h4>"),
                             
                             
                             
                                
                             shiny::HTML("<p align='justify'>
                                         The Synchory Index (Z) has been proposed to assess the degree of overlap between flowering individuals in a population. By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. 
                                   Z = 1 when germination of all the seeds occurs at the same time and Z = 0 when at least two seeds can germinate one each time. Z produces a number if and only if there are two seeds finishing the seed germination process at the same time. 
                                                                           Thus, the value of Z assessments is the grade of overlap between seed germination.
                                                                                  </p>"),
                             
                                 
                                img(src = "GRZ.png", height = 55, width = 240),
                                br(),
                                br(),
                                
                                span("Limits of measurements of the  germination variables; n: total number of seeds germinated.", style = "color:blue"),
                                br(),                                
                                br(),
                                
                                img(src = "SMRT.png", height = 200, width = 400),
                                
                                br(),
                                br(),
                                
                             
                             shiny::HTML("<h4><b>References</b></h4>"),
                             
                             
                               
shiny::HTML("<p>
            <br>                                <ol>
            <li>CARVALHO, M.; SANTANA, D. Emergencia de plantulas de  <i>Anacardium humile</i> 
                                  A. St.Hil.(Anacardiaceae) avaliada por meio de amostras pequenas. Revista Brasileira de, 2005.</li>
            
            <br>                         
            <li>FERREIRA MARQUES, F. R. et al. GerminaQuant : a new tool for germination measurements. 
            Journal of Seed Science, p. 1-8, 2015.</li>
            <br>
            <li>GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington: [s.n.].</li>
            <br>
            <li>JOOSEN, R. V. L. et al. germinator: a software package for high-throughput scoring and curve fitting of Arabidopsis seed germination. 
            The Plant Journal, v. 62, n. 1, p. 148-159, 22. abr. 2010.</li>
            <br>
            <li>LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.</li>
            <br>
            <li>MARCOS-FILHO, J. New approaches to seed vigor testing. Scientia Agricola, v. 55, n. spe, p. 27-33, 1998.</li>
            <br>
            <li>PENFIELD, S.; KING, J. Towards a systems biology approach to understanding seed dormancy and germination. 
            Proceedings. Biological sciences / The Royal Society, v. 276, n. 1673, p. 3561-9, 22 out. 2009.</li>
            <br>
            <li>RANAL, M. A. et al. Calculating germination measurements and organizing spreadsheets. 
            Revista Brasileira de Botanica, v. 32, n. 4, p. 849-855, 2009.</li>
            <br>
            <li>RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process?
            Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.</li>
            
            
            
            
            
            </ol></p>")                                 
                          
                                
                              )
                            )
                   ),
                   


# Import Data  ----------------------------------------------------------------

                   tabPanel("Data Import", icon = icon("file", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                fileInput('data', 'choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                
                                
                                textInput("SeedN", label = strong("col name: seeds number"), value = "NSeeds"),
                                
                                textInput("evalName", label = strong("prefix: evaluation days"), value = "Ev"),
                                
                              
                                tags$hr(),
                                checkboxInput('header', 'header', TRUE),
                                radioButtons('sep', 'separator',
                                             c(comma=',',
                                               semicolon=';',
                                               tab='\t'),
                                             ','),
                                radioButtons('quote', 'quote',
                                             c(none='',
                                               'double'='"',
                                               'single'="'"),
                                             '"')
                                
                                
                              ),
                              
                              mainPanel(
                                
                                tableOutput('contents')
                                
                              )
                            )
                   ),


# Germination Analisys  ----------------------------------------------------------------

                   tabPanel("Germination Analysis", icon = icon("leaf", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                              
                              h4("germination indices"),            
                                           
                              img(src = "germinaquant.png", height = 50, width = 50),   
                              downloadButton('downloadData', 'Download')

                              
                              ),
                              
                              
                              mainPanel(
                                
                                dataTableOutput("summary")
                                
                              )
                            )
                   ),
                   

# Statistical Analisys  ----------------------------------------------------------------


                   tabPanel("Statistical Analysis",icon = icon("stats", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                               
                                uiOutput('out1'),
                                uiOutput('out2'),
                                uiOutput('out3'),
                                uiOutput('out4'),
                                
                                h4('mean comparison table'),
                            
                                img(src = "germinaquant.png", height = 50, width = 50),   
                                downloadButton('downloadmc', 'Download'),
                                br()
                              ),
                              
                              mainPanel(
                                
                                h4('analysis of variance (ANOVA)'),
                                verbatimTextOutput("tbaov"),
                                
                                br(),
                                
                                h4("mean comparison test"),
                                br(),
                                tableOutput('MNC'),
                                br()
                                
                              )
                            )
                   ),

# Plot Graphics  ----------------------------------------------------------------

tabPanel("MultiPlot", icon = icon("area-chart", "fa-1x"),
         sidebarLayout(
           sidebarPanel(width = 3,
                        
                        h4("graphic labels"),
                        
                        textInput("lbmx", "Axis x", value = " "),
                        textInput("lbmy", "Axis y", value = " "),
                        textInput("lbml", "Legend", value = " ")
                        
                        
           ),
           
           mainPanel(
             
             br(),
             plotOutput("barplot"),
             br(),
             
             plotOutput("lineplot"),
             br(),
             
             plotOutput("boxplot"),
             br(),
             br()
             
             
           )
         )
),

# Germination In Time  ----------------------------------------------------------------

tabPanel("Germination InTime", icon = icon("equalizer", lib = "glyphicon"),
         sidebarLayout(
           sidebarPanel(width = 3,
             
             uiOutput('smvar'),  
             br(),
             
             textInput("lgnt", "unit time", value = "Time")
             
           ),
           
           mainPanel(
            
            h4("germination in time (percentage)"),
            
            plotOutput("GerInTimep"),
            br(),

            h4("germination in time (relative)"),
            
            plotOutput("GerInTimer"),
            br(),
            br()
            
            
             
           )
         )
),

navbarMenu("More", icon = icon("th-list", lib = "glyphicon"),

tabPanel("Tools", icon = icon("wrench", lib = "glyphicon"),
         
        sidebarLayout(
           sidebarPanel(width = 3, withMathJax(),
                        

               h4("calculos")         
                        
                        
           ),
           
           mainPanel(
             
             
             h4("osmotic potencial"),
             
             p("The osmotic pressure can be measured directly with an osmometer, 
               or it can be calculated from the solute concentration in the cell
               from the van't Hoff relation: \\(\\pi = -RTC\\) where \\(R\\) is the gas constant,
               \\(T\\) is the absolute temperature (in degrees Kelvin) and \\(C\\) is the solute concentration in Osmoles 
              \\(l^{-1}\\). At \\(25^{o}C\\), \\(RT\\) equals 2.5 litre-MPa per mole, and \\(\\pi\\) is in units of \\(Mpa\\)."),
             
             # Hence a concentration of 200 mOsmoles L-1 has an osmotic pressure of 0.5 MPa.
             
             br(),
             
             h4("water potencial"),
             
             p("The water pressure")
             
        
           )
           
           
         )
         
    
         ),


tabPanel("About", icon = icon("info-sign", lib = "glyphicon"),
         
         
         p("GerminaQuant is based in GerminaR R package, a highly interactive data analysis platform for germination analysis,
         tool develpment for the Plant Physiology Laboratory (UFPE). 
         It is result of a continuous effort to improve data collection, quality, analysis and open access publication. 
         The recent iteration simultaneously also represents efforts to unify best practices from experiences in germination data management.
         One of the main new characteristics of the current software development platform established is the web-based interface 
         which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops. 
         The aime is support the broader research community working on all aspects with germination studies.")

         
         )

  )

)
)







                            