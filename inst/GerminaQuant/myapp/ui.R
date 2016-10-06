library(shiny)
library(ggplot2)
library(GerminaR)

shinyUI(navbarPage("GerminaQuant",
                   
                   
                   
# Introduction ------------------------------------------------------------
                   
                   
                   tabPanel("Introduction",icon = icon("home", lib = "glyphicon"), 
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                
                                img(src = "germinaquant.png", height = 180, width = 180),
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
                                br()
                                
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
                              
                                shiny::HTML("<h4><b>Evaluation of seed germination process</b></h4>"),
                                
                       
                                
                              shiny::HTML("<p align='justify'> The physiology and seed technology have provided valuable tools for the production of high quality seed and treatments and storage conditions (MARCOS-FILHO, 1998). 
                                          In basic research, the seeds are studied exhaustively, and the approach of its biology is performed to fully exploit the dormancy and germination (PENFIELD; KING, 2009). 
                                          An important tool to indicate the performance of a seed lot is the precise quantification of germination through accurate analysis of the cumulative germination data (JOOSEN et al., 2010).
                                          Time, speed, homogeneity and synchrony are aspects that can be measured, and inform the dynamics of the germination process. 
                                          These characteristics are interesting not only for physiologists and seed technologists, but also for ecologist, since it is possible to predict the degree of success of the species, 
                                          based on the seed crop ability to redistribute germination over time, allowing the recruitment the part of the environment formed seedlings (RANAL; SANTANA, 2006).   </p>"),
                              
                              hr(),
                              
                              shiny::HTML("<h4><b> Germination Indices </b></h4>"),
                              
                              withMathJax(tableOutput("var")),
                              
                              
                              p("Germination variables evaluated in GerminaQuant and limits; where: \\(n_i\\), number of seed germinated in \\(i^{nth}\\) time ; \\(K\\), the last day of the avaliation process for germination"),
                             
                              br(),
                              
                              shiny::HTML("<h4><b>References</b></h4>"),
                             
                             
                               
shiny::HTML("<p>
            <br>                                <ol>
            <li>CARVALHO, M.; SANTANA, D. Emergencia de plantulas de  <i>Anacardium humile</i> 
                                  A. St.Hil.(Anacardiaceae) avaliada por meio de amostras pequenas. Revista Brasileira de, 2005.</li>
            
            <li>FERREIRA MARQUES, F. R. et al. GerminaQuant : a new tool for germination measurements. 
            Journal of Seed Science, p. 1-8, 2015.</li>

            <li>GOUVEA LABOURIAU, L. L. G. L. A germinacao das sementes. Washington: [s.n.].</li>
         
            <li>JOOSEN, R. V. L. et al. germinator: a software package for high-throughput scoring and curve fitting of Arabidopsis seed germination. 
            The Plant Journal, v. 62, n. 1, p. 148-159, 22. abr. 2010.</li>
         
            <li>LABOURIAU, L. G.; VALADARES, M. E. B. The germination of seeds. OEA, Washington, DC, 1983.</li>
         
            <li>MARCOS-FILHO, J. New approaches to seed vigor testing. Scientia Agricola, v. 55, n. spe, p. 27-33, 1998.</li>
          
            <li>PENFIELD, S.; KING, J. Towards a systems biology approach to understanding seed dormancy and germination. 
            Proceedings. Biological sciences / The Royal Society, v. 276, n. 1673, p. 3561-9, 22 out. 2009.</li>
          
            <li>RANAL, M. A. et al. Calculating germination measurements and organizing spreadsheets. 
            Revista Brasileira de Botanica, v. 32, n. 4, p. 849-855, 2009.</li>
        
            <li>RANAL, M. A.; SANTANA, D. G. DE. How and why to measure the germination process?
            Revista Brasileira de Botanica, v. 29, n. 1, p. 1-11, mar. 2006.</li>
            
            
            </ol></p>"),

br(),
br()
                          
                                
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

navbarMenu("more", icon = icon("th-list", lib = "glyphicon"),

tabPanel("osmotic tools", icon = icon("wrench", lib = "glyphicon"),
         
        sidebarLayout(
           sidebarPanel(width = 3, withMathJax(),
              
                                 
        tabsetPanel(type = "tabs", 
                    
                   
                    tabPanel("SALT",
                             
                             
                             br(),
                             
                             p(strong("salt (\\(g\\))")),
                             
                             textOutput("ops"),
                             
                             hr(),
                             
                             numericInput("vol", label = p("volumen (\\(L\\))"), value = 1.0, min = 0),
                             
                             numericInput("pre", label = p("presion (\\(MPa\\))"), value = -0.8, max = 0),
                             
                             numericInput("tem", label = p("temperature (\\(^{o}C\\))"), value = 25.0),
                             
                             numericInput("psm", label = p("molecular weight"), value = 58.4428, min = 0),
                             
                             numericInput("dis", label = p("salt dissociation constant"), value = 1.8, min = 0)
                             
                             
                             ), 
                    
                    tabPanel("PEG 600",
                             
                             br(),
                             
                             p(strong("PEG 600 (\\(g\\))")),
                             
                             textOutput("opp"),
                             
                             hr(),
                             
                             numericInput("volp", label = p("volumen (\\(L\\))"), value = 1.0, min = 0),
                             
                             numericInput("prep", label = p("presion (\\(MPa\\))"), value = -0.8, max = 0),
                             
                             numericInput("temp", label = p("temperature (\\(^{o}C\\))"), value = 25.0)
                             
                             )
                    
                    )
        

           ),
           
           mainPanel( 
             
             
             h4("osmotic potencial"),
             
             br(),
             
             p("The osmotic potencial, can be measured directly with an osmometer, or it can be calculated from the solute concentration."),
             
             hr(),
             
             p("For a salt, you can use the van't Hoff relation: $$\\psi_s = -RTC_i$$ where: \\(R\\) is the gas constant (i.e. \\(0.0083 L/atm/mol/K\\)), \\(T\\) is the absolute temperature in degrees in Kelvin (\\(273.15^{o}C\\)), 
             \\(C\\) is the solute concentration in \\(mol*L^{-1}\\), and \\(i\\) is the dissociation constant of the salt. (i.e. \\(NaCl = 1.8, KCl = 1.8, CaCl_2 = 2.4, sacarose = 1\\)). The unit for \\(\\psi_s\\) is \\(MPa\\)"), 
             
             hr(),

             p("For", em("PEG 6000"), "the osmotic potentials can be calculated as described by", a("Michel and Kaufmann (1973):", href = "http://www.plantphysiol.org/content/51/5/914.abstract", target="_blank"), "$$\\psi_s = -(1.18*10^{-2})C - (1.18*10^{-4})C^2 + (2.67*10^{-4})CT + (8.39*10^{-7})C^2T$$ 
             where: \\(C\\) is the concentration of", em("PEG 6000"), "in \\(g*L^{-1}\\) and \\(T\\) is the temperature in degrees \\(^{o}C\\).  The unit for \\(\\psi_s\\) is \\(bar (0.1 MPa)\\)."),
           
             br()
             

             )
           
           
         )
         
    
         ),

tabPanel("user manual", icon = icon("book", lib = "glyphicon"),
         
         
         htmlOutput("usmn")  
         
),


tabPanel("data sample", icon = icon("duplicate", lib = "glyphicon"),
         
         
         htmlOutput("dtsm")  
         
),



tabPanel("about", icon = icon("info-sign", lib = "glyphicon"),
         
         h4(strong("GerminaQuant")),
         
         p("GerminaQuant is based in GerminaR R package, a highly interactive data analysis platform for germination analysis,
         tool develpment for the Plant Physiology Laboratory (UFPE). 
         It is result of a continuous effort to improve data collection, quality, analysis and open access publication. 
         The recent iteration simultaneously also represents efforts to unify best practices from experiences in germination data management.
         One of the main new characteristics of the current software development platform established is the web-based interface 
         which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops. 
         The aime is support the broader research community working on all aspects with germination studies."),
        
         p(strong("project name:"), "GerminaR - An R Package for germination analysis process with interactive web app 'GerminaQuant'"),
         p(strong("stable release:"), a("GerminaR", href = "cran.r-project.org/package = germinar", target="_blank" )),
         p(strong("interactive application:"), a("GerminaQuant", href = "https://flavjack.shinyapps.io/germinaquant/", target="_blank" )),
         p(strong("user manual:"), a("GerminaQuant", href = "https://bookdown.org/flavjack/germinaquant/", target="_blank" )),
         p(strong("webpage:"), a("LEV", href = "https://www.ufpe.br/lev/index.php", target="_blank" )),
         p(strong("issue tracker:"), a("github", href = "https://github.com/Flavjack/GerminaR/issues", target="_blank" )),
         p(strong("operating systems:"), "independient of the platform"),
         p(strong("programing language:"), "R & html"),
         
         hr(),
         
         p(strong("If you have any questions, suggestion or comment, you can write an email for the authors, thank you!!, and enjoy GerminaQuant.")),
         
         hr(),
         
         h4(strong("Authors")),
         
                p(strong("Flavio Lozano Isla "), "(", a("flavjack@gmail.com"), "). Department of Botany, Rural Federal University of Pernambuco, Recife, Brazil"),
                
                p(strong("Omar Benites Alfaro "), "(",a("obacc07@gmail.com"), "). Computing Research Department for Plant Breeding, International Potato Center, Lima, Peru"),
                
                p(strong("Marcelo Francisco Pompelli "), "(",a("marcelo.pompelli@ufpe.br"), "). Department of Biology, Federal University of Pernambuco, Recife, Brazil"),

         
         br(),
         br()
         
         )

  )

)
)

                            