library(shiny)
library(ggplot2)
library(GerminaR)


shinyUI(navbarPage("GerminaQuant",
                   
# Introduction ------------------------------------------------------------
                   
                   
                   tabPanel("Introduction",
                            sidebarLayout(
                              sidebarPanel(
                                
                                p("GerminaQuant is web application based in R and Shiny."),
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
                                "GerminaQuant is a product of ", 
                                span("Ecophysiology Laboratory (UFPE)", style = "color:blue"),
                                p("Visit the ",
                                  a("LEV homepage", 
                                    href = "https://www.ufpe.br/lev/")),
                                br(),
                                strong("Authors"),
                                br(),
                                div("Lozano-Isla, Flavio ", style = "color:green"),
                                div("Benites-Alfaro, Omar ", style = "color:green"),
                                div("Pompelli, Marcelo F.", style = "color:green")
                                
                            
                                
                              ),
                              
                              mainPanel(
                                
                                
                                p("GerminaQuant application allows make the calculation for the germination variables ", em("incredibly easy"), 
                                  " in an interactive applications build with R and Shiny. GerminaQuant app is live!. Outputs change instantly as users modify inputs, without requiring a reload the app."),
                                
                                h4("Features"),
                                
                                p("Allow calculate the princiapal Germination Variables."),
                                p("Statistical Analysis for Germination Variables."),
                                p("Easy way to plot the results."),

                                br(),
                                
                                h4("Evaluation of seed germination process"),
                                
                                p("The physiology and seed technology have provided valuable tools for the production of high quality seed and treatments and storage conditions (MARCOS-FILHO, 1998).
                                  In basic research, the seeds are studied exhaustively, and the approach of its biology is performed to fully exploit the dormancy and germination (PENFIELD; KING, 2009).
                                  An important tool to indicate the performance of a seed lot is the precise quantification of germination through accurate analysis of the cumulative germination data (JOOSEN et al., 2010). 
                                  Time, speed, homogeneity and synchrony are aspects that can be measured, and inform the dynamics of the germination process. 
                                  These characteristics are interesting not only physiologists and seed technologists, but also to environmentalists, since it is possible to predict the degree of success of the species, 
                                  based on the seed crop ability to redistribute germination over time, allowing the recruitment the part of the environment formed seedlings (RANAL; SANTANA, 2006)."),
                                
                                h4("Germination (g)"),
                                
                                p("According GOUVEA LABOURIAU (1983), the germinability of a sample of is the percentage of seeds in which the seed germination process comes to an end, 
                                  in experimental conditions by the seminal intrauterine growth resulting protrusion (or emergence) of a living embryo. In general, it is presented as percentage, 
                                  accompanied by some degree of dispersion, but it is possible to use proportions to one or more samples may be subjected to statistical tests (CARVALHO; SANTANA, 2005)"),
                                
                                img(src = "GRM.png", height = 60, width = 120),
                                br(),
                                
                                h4("Mean Germination Time (t)"),
                                
                                p("It was proposed by Haberlandt in 1875. It is calculated as the weighted average germination time. 
                                  The number of germinated seeds at the intervals established for the collection of data is used as weight. 
                                  It is expressed in terms of the same units of time used in the germination count (CZABATOR, 1962)"),
                                
                                img(src = "MGT.png", height = 60, width = 120),
                                br(),
                                
                                h4("Mean Germination Rate (v)"),
                                
                                p("The average speed of germination is defined as the reciprocal of the average time germination (RANAL; SANTANA, 2006)"),
                                
                                img(src = "MGR.png", height = 50, width = 80),
                                br(),
                                
                                h4("Uncertainty Index (u)"),
                                
                                p("The uncertainty index (u) is an adaptation of Shannon index measures the degree of uncertainty in predicting the informational entropy or uncertainty associated with the distribution of the relative frequency of germination (GOUVEA LABOURIAU 1983; LABOURIAU; VALADARES, 1983). 
                                  Low values of u indicate frequencies with short peaks, i.e. the more concentrated the germination in time. Just a germinated seed changes the value of u. This means that u measures the degree of germination scattering."),
                                
                                img(src = "UCI.png", height = 60, width = 240),
                                br(),
                                
                                h4("Synchrony Index (Z)"),
                                
                                p("The Synchory Index (Z) has been proposed to assess the degree of overlap between flowering individuals in a population. By adopting the idea expressed by PRIMACK, R.B. (1980) the synchrony of one seed with other included in the same replication. 
                                  Z = 1 when germination of all the seeds occurs at the same time and Z = 0 when at least two seeds can germinate one each time. Z produces a number if and only if there are two seeds finishing the seed germination process at the same time. 
                                  Thus, the value of Z assessments is the grade of overlap between seed germination."),
                                
                                img(src = "GRZ.png", height = 55, width = 240),
                                br(),
                                br(),
                                
                                span("Limits of measurements of the  germination variables; n: total number of seeds germinated.", style = "color:blue"),
                                br(),
                                img(src = "SMRT.png", height = 200, width = 400),
                                
                                br(),
                                br(),
                                
                                h4("References"),
                                
                                p("CARVALHO, M.; SANTANA, D. Emergencia de plantulas de", em("Anacardium humile"), 
                                  "A. St.Hil.(Anacardiaceae) avaliada por meio de amostras pequenas. Revista Brasileira de, 2005."),
                                
                                p("CZABATOR, F. J. Germination value: an index combining speed and completeness of pine seed germination. 
                                  Forest Science, v. 8, n. 4, p. 386-396, 1962."),
                                
                                p("FERREIRA MARQUES, F. R. et al. GerminaQuant : a new tool for germination measurements. 
                                  Journal of Seed Science, p. 1-8, 2015."),
                                
                                p("GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington: [s.n.]."),
                               
                                p("JOOSEN, R. V. L. et al. germinator: a software package for high-throughput scoring and curve fitting of Arabidopsis seed germination. 
                                  The Plant Journal, v. 62, n. 1, p. 148-159, 22. abr. 2010."),
                                
                                p("LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983."),
                                
                                p("MARCOS-FILHO, J. New approaches to seed vigor testing. Scientia Agricola, v. 55, n. spe, p. 27-33, 1998."),
                                
                                p("PENFIELD, S.; KING, J. Towards a systems biology approach to understanding seed dormancy and germination. 
                                  Proceedings. Biological sciences / The Royal Society, v. 276, n. 1673, p. 3561-9, 22 out. 2009."),
                                
                                p("RANAL, M. A. et al. Calculating germination measurements and organizing spreadsheets. 
                                  Revista Brasileira de Botanica, v. 32, n. 4, p. 849-855, 2009. "),
                                
                                p("RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process?
                                  Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006."),
                                br()
                                
                          
                                
                              )
                            )
                   ),
                   


# Import Data  ----------------------------------------------------------------

                   tabPanel("Data Import",
                            sidebarLayout(
                              sidebarPanel(
                                
                                fileInput('data', 'Choose CSV File',
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                
                                
                                textInput("SeedN", label = strong("Colom with seeds number"), value = "NSeeds"),
                                
                                textInput("evalName", label = strong("Prefix of evaluation days"), value = "Ti"),
                                
                                numericInput("freq", label = strong("Frequency of evaluation"), value = 1),
                                
                                
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

                   tabPanel("Germination Analysis",
                            sidebarLayout(
                              sidebarPanel(
                              
                              img(src = "germinaquant.png", height = 80, width = 80),   
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
                              "CVL",
                              br(),
                              span("Velocity coefficient of germination", style = "color:blue"),
                              br(),
                              "GRU",
                              br(),
                              span("Germination uncertainty index", style = "color:blue"),
                              br(),
                              "GSI",
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


                   tabPanel("Statistical Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                
                               
                                uiOutput('out1'),
                                uiOutput('out2'),
                                br(),
                                br(),
                                
                                h4("Graphic labels"),
                                
                                textInput("lbmx", "Axis x", value = " "),
                                textInput("lbmy", "Axis y", value = " "),
                                textInput("lbml", "Legend", value = " "),
                                br(),
                                br(),
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

tabPanel("Germination InTime",
         sidebarLayout(
           sidebarPanel(
             
             uiOutput('smvar')  
             
             
             
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

                   tabPanel("Graphics",
                            sidebarLayout(
                              sidebarPanel(
                                
                                h4("Graphics variables"),
                                
                                uiOutput('ex'),
                                uiOutput('eg'),
                                uiOutput('ey'),
                                
                                
                                br(),
                                br(),
                                
                                h4("Graphics labels"),
                                
                                textInput("lbx", "Axis x", value = " "),
                                textInput("lby", "Axis y", value = " "),
                                textInput("lbg", "Legend", value = " ")
                                
                                
            
                              ),
                              
                              mainPanel(
                                
                                h4("Boxplot"),
                                br(),
                                plotOutput("Boxplot"),
                                br(),
                                h4("Dotplot"),
                                br(),
                                plotOutput("Dotplot"),
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






                            