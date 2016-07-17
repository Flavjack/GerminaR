library(shiny)
library(ggplot2)
library(GerminaR)


shinyUI(navbarPage("GerminaQuant",
                   
# Introduction ------------------------------------------------------------
                   
                   
                   tabPanel("Introduction",icon = icon("home", lib = "glyphicon"), 
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                p("GerminaQuant is web application based in R."),
                                p("You can use the app in your desktop installing the package", em("GerminaR"), "in the usual way from your R console:"),
                                code('install.packages(GerminaR)'),
                                br(),
                                br(),
                                p("For use the interactive app:"),
                                code("library(GerminaR)"),
                                br(),
                                code("runGerminaQuant()"),
                                br(),
                                br(),
                                img(src = "germinaquant.png", height = 125, width = 125),
                                br(),
                                br(),
                                span(
                                  p("GerminaQuant is a product of ",
                                    a("Plant Physiology Laboratory (UFPE)", 
                                      href = "https://www.ufpe.br/lev/", target="_blank")), style = "color:black"),
                                
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
                                
                                
                              shiny::HTML("<p align='justify'>GerminaQuant application allows make the calculation for the germination variables <i>incredibly easy</i> 
                                            in an interactive applications build with R and Shiny package. GerminaQuant app is reactive!. Outputs change instantly as users modify inputs, without requiring a reload the app.</p>"),

                                shiny::HTML("<h4><b>Features</b></h4>"),
                                
                                shiny::HTML("<p>
                                            <ol>
                                            <li>Allow calculate the princiapal Germination Variables.</li>
                                            <li>Statistical Analysis for Germination Variables.</li>
                                            <li>Easy way to plot the results.</li>
                                            </ol>
                                            </p>"),
                                
                               # p("Allow calculate the princiapal Germination Variables."),
                              #  p("Statistical Analysis for Germination Variables."),
                               # p("Easy way to plot the results."),

                                br(),
                                
                                shiny::HTML("<h4><b>Evaluation of seed germination process</b></h4>"),
                                
                              #  h4("Evaluation of seed germination process"),
                                
                              shiny::HTML("<p align='justify'> The physiology and seed technology have provided valuable tools for the production of high quality seed and treatments and storage conditions (MARCOS-FILHO, 1998).
                                          In basic research, the seeds are studied exhaustively, and the approach of its biology is performed to fully exploit the dormancy and germination (PENFIELD; KING, 2009).
                                          An important tool to indicate the performance of a seed lot is the precise quantification of germination through accurate analysis of the cumulative germination data (JOOSEN et al., 2010). 
                                          Time, speed, homogeneity and synchrony are aspects that can be measured, and inform the dynamics of the germination process. 
                                          These characteristics are interesting not only for physiologists and seed technologists, but also to environmentalists, since it is possible to predict the degree of success of the species, 
                                          based on the seed crop ability to redistribute germination over time, allowing the recruitment the part of the environment formed seedlings (RANAL; SANTANA, 2006).   </p>"),
                              
#                                 p("The physiology and seed technology have provided valuable tools for the production of high quality seed and treatments and storage conditions (MARCOS-FILHO, 1998).
#                                   In basic research, the seeds are studied exhaustively, and the approach of its biology is performed to fully exploit the dormancy and germination (PENFIELD; KING, 2009).
#                                   An important tool to indicate the performance of a seed lot is the precise quantification of germination through accurate analysis of the cumulative germination data (JOOSEN et al., 2010). 
#                                   Time, speed, homogeneity and synchrony are aspects that can be measured, and inform the dynamics of the germination process. 
#                                   These characteristics are interesting not only physiologists and seed technologists, but also to ecologists, since it is possible to predict the degree of success of the species, 
#                                   based on the seed crop ability to redistribute germination over time, allowing the recruitment the part of the environment formed seedlings (RANAL; SANTANA, 2006)."),
#                                 
                              shiny::HTML("<h4><b>Germination (g)</b></h4>"),
                              
                              
                              #  h4("Germination (g)"),
                              shiny::HTML("<p align='justify'> According GOUVEA LABOURIAU (1983), the germinability of a sample of is the percentage of seeds in which the seed germination process comes to the end, 
                                          in experimental conditions by the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo. In general, it is presented as percentage, 
                                          accompanied by some degree of dispersion, but it is possible to use proportions to one or more samples may be subjected to statistical tests (CARVALHO; SANTANA, 2005)</p>"),

#   
#                                 p("According GOUVEA LABOURIAU (1983), the germinability of a sample is the percentage of seeds in which the seed germination process comes to ther end, 
#                                   in experimental conditions by the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo. In general, it is presented as percentage, 
#                                   accompanied by some degree of dispersion, but it is possible to use proportions to one or more samples may be subjected to statistical tests (CARVALHO; SANTANA, 2005)"),
#                                 
                                img(src = "GRM.png", height = 60, width = 120),
                                br(),
                                
                              shiny::HTML("<h4><b>Mean Germination Time (t)</b></h4>"),
                              
                               # h4("Mean Germination Time (t)"),
                              shiny::HTML("<p align='justify'>It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. 
                                          The number of germinated seeds at the intervals established for the collection of data is used as weight. 
                                          It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962)</p>"),          

  
#                                 p("It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. 
#                                   The number of germinated seeds at the intervals established for the collection of data is used as weight. 
#                                   It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962)"),
                                
                                img(src = "MGT.png", height = 60, width = 120),
                                br(),
                              
                                   
                              shiny::HTML("<h4><b>Mean Germination Rate (v)</b></h4>"),
                              
                              #  h4("Mean Germination Rate (v)"),
                              
                                shiny::HTML("<p align='justify'>The average speed of germination is defined as the reciprocal of the average 
                                              time germination (RANAL; SANTANA, 2006)</p>"),  
   
                                #p("The average speed of germination is defined as the reciprocal of the average time germination (RANAL; SANTANA, 2006)"),
                                
                                img(src = "MGR.png", height = 50, width = 80),
                                br(),
                                
                              shiny::HTML("<h4><b>Uncertainty Index (u)</b></h4>"),
                              
                             #   h4("Uncertainty Index (u)"),
                                
                              shiny::HTML("<p align='justify'> The uncertainty index (u) is an adaptation of Shannon index measures the 
                                            degree of uncertainty in predicting the informational entropy or uncertainty associated with the 
                                            distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). 
                                  Low values of u indicate frequencies with short peaks, i.e. the more concentrated the germination in time. 
                                          Just a germinated seed changes the value of u. This means that u measures the degree of germination scattering.</p>"),

#                                 p("The uncertainty index (u) is an adaptation of Shannon index measures the degree of uncertainty in predicting the informational entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). 
#                                   Low values of u indicate frequencies with short peaks, i.e. the more concentrated the germination in time. Just a germinated seed changes the value of u. This means that u measures the degree of germination scattering."),
#                                 
                                img(src = "UCI.png", height = 60, width = 240),
                                br(),
                                
                             shiny::HTML("<h4><b>Synchrony Index (Z)</b></h4>"),
                             
                             
                             #   h4("Synchrony Index (Z)"),
                                
                             shiny::HTML("<p align='justify'>
                                         The Synchory Index (Z) has been proposed to assess the degree of overlap between flowering individuals in a population. By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. 
                                   Z = 1 when germination of all the seeds occurs at the same time and Z = 0 when at least two seeds can germinate one each time. Z produces a number if and only if there are two seeds finishing the seed germination process at the same time. 
                                                                           Thus, the value of Z assessments is the grade of overlap between seed germination.
                                                                                  </p>"),
                             
#                                 p("The Synchory Index (Z) has been proposed to assess the degree of overlap between flowering individuals in a population. By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. 
#                                   Z = 1 when germination of all the seeds occurs at the same time and Z = 0 when at least two seeds can germinate one each time. Z produces a number if and only if there are two seeds finishing the seed germination process at the same time. 
#                                   Thus, the value of Z assessments is the grade of overlap between seed germination."),
#                                 
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
                             
                             
                                #h4("References"),
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
            
            
            
            
            
            </ol></p>")#,
#                                 
#                                 p("CZABATOR, F. J. Germination value: an index combining speed and completeness of pine seed germination. 
#                                   Forest Science, v. 8, n. 4, p. 386-396, 1962."),
#                                 
#                                 p("FERREIRA MARQUES, F. R. et al. GerminaQuant : a new tool for germination measurements. 
#                                   Journal of Seed Science, p. 1-8, 2015."),
#                                 
#                                 p("GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington: [s.n.]."),
#                                
#                                 p("JOOSEN, R. V. L. et al. germinator: a software package for high-throughput scoring and curve fitting of Arabidopsis seed germination. 
#                                   The Plant Journal, v. 62, n. 1, p. 148-159, 22. abr. 2010."),
#                                 
#                                 p("LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983."),
#                                 
#                                 p("MARCOS-FILHO, J. New approaches to seed vigor testing. Scientia Agricola, v. 55, n. spe, p. 27-33, 1998."),
#                                 
#                                 p("PENFIELD, S.; KING, J. Towards a systems biology approach to understanding seed dormancy and germination. 
#                                   Proceedings. Biological sciences / The Royal Society, v. 276, n. 1673, p. 3561-9, 22 out. 2009."),
#                                 
#                                 p("RANAL, M. A. et al. Calculating germination measurements and organizing spreadsheets. 
#                                   Revista Brasileira de Botanica, v. 32, n. 4, p. 849-855, 2009. "),
#                                 
#                                 p("RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process?
#                                   Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006."),
#                                 br()
#                                 
                          
                                
                              )
                            )
                   ),
                   


# Import Data  ----------------------------------------------------------------

                   tabPanel("Data Import", icon = icon("file", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                fileInput('data', 'Choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                
                                
                                textInput("SeedN", label = strong("Column with seeds number"), value = "NSeeds"),
                                
                                textInput("evalName", label = strong("Prefix of evaluation days"), value = "Ev"),
                                
                              
                                tags$hr(),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"')
                                
                                
                              ),
                              
                              mainPanel(
                                
                                tableOutput('contents')
                                
                              )
                            )
                   ),


# Germination Analisys  ----------------------------------------------------------------

                   tabPanel("Germination Analysis", icon = icon("text-background", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                              
                              img(src = "germinaquant.png", height = 60, width = 60),   
                              downloadButton('downloadData', 'Download'),
                              br(),
                              br(),
                              strong("Abbreviations"),
                              br(),
                              br(),
                              "GRS",
                              br(),
                              span("Germinated seeds", style = "color:blue"),
                              br(),
                              "GRP",
                              br(),
                              span("Germination percentage", style = "color:blue"),
                              br(),
                              "ASG",
                              br(),
                              span("Arcsin of germination", style = "color:blue"),
                              br(),
                              "MGT",
                              br(),
                              span("Mean germination time", style = "color:blue"),
                              br(),
                              "MGR",
                              br(),
                              span("Mean germination rate", style = "color:blue"),
                              br(),
                              "GSP",
                              br(),
                              span("Germination Speed", style = "color:blue"),
                              br(),
                              "UNC",
                              br(),
                              span("Germination uncertainty index", style = "color:blue"),
                              br(),
                              "SYN",
                              br(),
                              span("Germination synchrony index", style = "color:blue"),
                              br(),
                              "VGT",
                              br(),
                              span("Variance of mean germination time", style = "color:blue"),
                              br(),
                              "SDG",
                              br(),
                              span("Standard desviation of mean germination time", style = "color:blue"),
                              br(),
                              "CVG",
                              br(),
                              span("Coefficient of variance of mean germination time", style = "color:blue"),
                              br()
                              
                        
                          
                              ),
                              
                              
                              mainPanel(
                                
                                tableOutput("summary")
                                
                              )
                            )
                   ),
                   

# Statistical Analisys  ----------------------------------------------------------------


                   tabPanel("Statistical Analysis",icon = icon("stats", lib = "glyphicon"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                               
                                uiOutput('out1'),
                                uiOutput('out2'),
                                br(),
                                br(),
                                
                                h4("Graphic labels"),
                                
                                textInput("lbmx", "Axis x", value = " "),
                                textInput("lbmy", "Axis y", value = " "),
                                textInput("lbml", "Legend", value = " "),
                                br(),
                                br(),strong("Abbreviations"),
                                br(),
                                br(),
                                "GRS",
                                br(),
                                span("Germinated seeds", style = "color:blue"),
                                br(),
                                "GRP",
                                br(),
                                span("Germination percentage", style = "color:blue"),
                                br(),
                                "ASG",
                                br(),
                                span("Arcsin of germination", style = "color:blue"),
                                br(),
                                "MGT",
                                br(),
                                span("Mean germination time", style = "color:blue"),
                                br(),
                                "MGR",
                                br(),
                                span("Mean germination rate", style = "color:blue"),
                                br(),
                                "GSP",
                                br(),
                                span("Germination Speed", style = "color:blue"),
                                br(),
                                "UNC",
                                br(),
                                span("Germination uncertainty index", style = "color:blue"),
                                br(),
                                "SYN",
                                br(),
                                span("Germination synchrony index", style = "color:blue"),
                                br(),
                                "VGT",
                                br(),
                                span("Variance of mean germination time", style = "color:blue"),
                                br(),
                                "SDG",
                                br(),
                                span("Standard desviation of mean germination time", style = "color:blue"),
                                br(),
                                "CVG",
                                br(),
                                span("Coefficient of variance of mean germination time", style = "color:blue"),
                                br()
                               
                              
                              ),
                              
                              mainPanel(
                                
                                h4('ANOVA Table'),
                                verbatimTextOutput('aovSummary'),
                                
                                br(),
                                plotOutput("Barplot"),
                                br(),
                              
                                h4("Mean Comparison Test"),
                                
                                verbatimTextOutput('MNC'),
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
             
             h4("Graphics labels"),
             br(),
             
             textInput("lgnt", "Unit time", value = "Time")
             
           ),
           
           mainPanel(
            
            h4("Germination in Time"),
            
            plotOutput("GerInTimep"),
            br(),

            h4("Relative Germination in Time"),
            
            plotOutput("GerInTimer"),
            br(),
            br()
            
            
             
           )
         )
),



# Plot Graphics  ----------------------------------------------------------------

                   tabPanel("Box Plot", icon = icon("area-chart", "fa-1x"),
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                h4("Graphics variables"),
                                
                                uiOutput('ex'),
                                uiOutput('eg'),
                                uiOutput('ey'),
                                
                                
                                br(),
                              
                                h4("Graphics labels"),
                                
                                textInput("lbx", "Axis x", value = " "),
                                textInput("lby", "Axis y", value = " "),
                                textInput("lbg", "Legend", value = " ")
                                
                                
            
                              ),
                              
                              mainPanel(
                                
                                br(),
                                plotOutput("Boxplot"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()
                                
                               
                              )
                            )
                   )
                   
)
)






                            