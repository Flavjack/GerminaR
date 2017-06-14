# GerminaQuant -----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)
library(agricolae)
library(GerminaR)


shinyUI(dashboardPage(skin = "green",


    dashboardHeader(title = "GerminaQuant for R"),

# Sider -------------------------------------------------------------------

    dashboardSidebar(

      sidebarMenu(
        menuItem("Presentacion", tabName = "intro", icon = icon("home")),
        menuItem("User Manual", tabName = "manual", icon = icon("book")),
        menuItem("Fieldbook", tabName = "fieldbook", icon = icon("leaf")),
        menuItem("Germination", tabName = "germination", icon = icon("leaf")),
        menuItem("Box plot", tabName = "outlier", icon = icon("search")),
        #menuItem("Multivariate", tabName = "multv", icon = icon("paperclip")),
        #menuItem("Regression", tabName = "regression", icon = icon("random")),
        menuItem("Statistics", tabName = "stat", icon = icon("pie-chart")),
        menuItem("Graphics", tabName = "graph", icon = icon("tint")),
        menuItem("Intime", tabName = "germint", icon = icon("hourglass")),
        menuItem("Tools", tabName = "tools", icon = icon("wrench"))
      )


    ),



    dashboardBody(

      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),

      tabItems(

        

# presentacion ------------------------------------------------------------


        tabItem(tabName = "intro",

                shiny::fluidRow(

                box(
                  title = "Presentacion",
                  width = 3,
                  status = "primary",
                  solidHeader = T,

                  
                  img(src = "germinaquant.png", height = 180, width = 180),
                  span(
                    p(a("Plant Physiology Laboratory (UFPE)", 
                        href = "https://www.ufpe.br/lev/", 
                        target="_blank")), 
                    style = "color:black"),
                  
                  p("GerminaQuant for R is web application based in R, you can use the app in your desktop installing the package", em("GerminaR")),
                  code('install.packages(GerminaR)'),
                  br(),
                  br(),
                  p("For use the interactive app:"),
                  code("library(GerminaR)"),
                  br(),
                  code("GerminaQuant()"),
                  br(),
                  br()
                  
                ),


                box(width = 5, 
                    title = "Characteristics", 
                    status = "danger",
                    solidHeader = T,
                    
                    
                    p("GerminaQuant for R is based in GerminaR R package, a highly interactive data analysis platform for germination analysis,
                      tool develpment for the Plant Physiology Laboratory (UFPE).
                      It is result of a continuous effort to improve data collection, quality, analysis and open access publication.
                      The recent iteration simultaneously also represents efforts to unify best practices from experiences in germination data management.
                      One of the main new characteristics of the current software development platform established is the web-based interface
                      which provides also a highly interactive environment. It could be used both online and offline and on desktop as well as tablets and laptops.
                      The aime is support the broader research community working on all aspects with germination studies."),
                    
                    shiny::HTML("<h5><b>Features</b></h5>"),
                    
                    shiny::HTML("<p>
                                <ol>
                                <li>Allow calculate the princiapal germination variables.</li>
                                <li>Statistical analysis for germination variables.</li>
                                <li>Easy way to plot the results.</li>
                                </ol>
                                </p>"),

                    p(strong("Project name:"), "GerminaR - An R Package for germination analysis process with interactive web app 'GerminaQuant for R'"),
                    p(strong("Stable release:"), a("GerminaR", href = "https://cran.r-project.org/web/packages/GerminaR/index.html", target="_blank" )),
                    p(strong("Issue tracker:"), a("github", href = "https://github.com/Flavjack/GerminaR/issues", target="_blank" )),
                    p(strong("Operating systems:"), "independient of the platform"),
                    p(strong("Programing language:"), "R & html")
                    
                    
                ),
                    
                
                box(
                  title = "Contributors",
                  width = 4,
                  status = "success",
                  solidHeader = T,

                  p(
                    strong("Flavio Lozano Isla "),
                    br(),
                    a("< flavjack@gmail.com >"),
                    br(),
                    code("Universidade Federal Rural de Pernambuco, Brazil.")
                    ),

                  p(
                    strong("Omar Benites Alfaro"),
                    br(),
                    a("< obacc07@gmail.com >"),
                    br(),
                    code("Centro Internacional de la Papa (CIP), Peru.")
                  ),
                  
                  p(
                    strong("Denise Garcia de Santana"),
                    br(),
                    a("< denise.santana@ufu.br >"),
                    br(),
                    code("Universidade Federal de Uberlandia, Brazil.")
                  ),
                  
                  p(
                    strong("Marli A. Ranal"),
                    br(),
                    a("< ranal@ufu.br >"),
                    br(),
                    code("Universidade Federal de Uberlandia, Brazil.")
                  ),
                  

                  p(
                    strong("Marcelo Francisco Pompelli"),
                    br(),
                    a("< mpompelli@gmail.com >"),
                    br(),
                    code("Universidade Federal de Pernambuco, Brazil.")
                  ),


                  hr(),

                  p(strong("If you have any question, commment or sugestion you can write a email for us, enjoy GerminaQuant for R!!"))



                )
                
                )

        ),



# User Manual -------------------------------------------------------------
    
          tabItem(tabName = "manual",
            
            
                  htmlOutput("gb")  
            
          ),



# fieldbook -------------------------------------------------------------


        tabItem(tabName = "fieldbook",


        box(

          status = "info",
          width = 12,
          background = "black",


          column(width = 6,

           h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),

           textInput("fbdt",
             label = NULL ,
             width = "100%",
             value = "https://docs.google.com/spreadsheets/d/1QziIXGOwb8cl3GaARJq6Ez6aU7vND_UHKJnFcAKx0VI/edit#gid=137089581")


          ),


          column(width = 4,

            h4(icon("book"), "Excel file (.xlsx)", width = "100%"),

            fileInput('impdata',
              label = NULL,
              accept = c(".xlsx"))

          ),

          column(width = 1,

            h4("Sheet", width = "100%"),

            numericInput("sheetdt", label = NULL, value = 1, step = 1, min = 1)

          ),

          column(width = 1,

            h4( "Update", width = "100%"),

            actionButton(inputId = "reload", label = "", icon("refresh"), width = "100%")

          )


        ),


        shiny::fluidRow(
          
     
        box(

          status = "danger",
          solidHeader = T,
          width = 10,

        # DT::dataTableOutput('fbook')
        htmlOutput("fbook")


        ),



# Germination parameters --------------------------------------------------


        box(
          
          status = "danger",
          solidHeader = T,
          width = 2,
          title = 'Parameters',
          
          
          textInput("SeedN", label = strong("Seeds (col name)"), value = "seeds"),
          
          textInput("evalName", label = strong("Evaluations (prefix)"), value = "D"),
          
  
          uiOutput("filter_01"),
          
          uiOutput("filter_fact01"),
          
          
          uiOutput("filter_02"),
          
          uiOutput("filter_fact02")
          
                 
          )
          
        )

        ),



# Germination analisys ----------------------------------------------------

        tabItem(tabName = "germination",
                
        shiny::fluidRow(
          
          box(width = 12,
              
              dataTableOutput("summary")
              
          )   
          
        )
     
        
        
        ),




# outliers ----------------------------------------------------------------

        tabItem(tabName = "outlier",

          box(width = 10, background = "black",

                    column(width = 4,

                      uiOutput("bpy")

                    ),


                    column(width = 4,

                      uiOutput("bpx")

                    ),


                    column(width = 4,

                      uiOutput("bpz")


                    ),



                    column(width = 4,

                      textInput(inputId ="bply", label = "Y label", value = "")


                    ),


                    column(width = 4,


                      textInput(inputId ="bplx", label = "X label", value = "")


                    ),


                    column(width = 4,

                      textInput(
                        inputId ="bplz",
                        label = "Legend label",
                        value = "")


                    )

          ),


          box(width = 2, background = "black",


            column(width = 12,

              numericInput(
                inputId ="bpbrk",
                label = "Axis brake",
                value = NA)

            ),


            column(width = 12,

              numericInput(
                inputId ="bpsize",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1)


            )


            ),


          shiny::fluidRow(
          
          box(width = 12,

          plotOutput("boxplot")


          )
          
          )

        ),


# multivariate ------------------------------------------------------------

        tabItem(tabName = "multv",

         box(width = 6,

           column(width = 3,

             h5(icon("book"), "Correlation", width = "100%")

           ),


           column(width = 2,

            numericInput("corsig",
               label = "Significance",
              value = 0.05,
              min = 0,
              max = 5,
              step = 0.01)

           ),


           column(width = 2,

             numericInput("cor_font",
               label = "Font",
               value = 1,
               min = 0,
               step = 0.1)


           ),

            column(width = 5,

              textInput("corcol",
                label = "Color",
                value = "#DD5143 #F38A78 #68C7EC #00A0DC"
               )

            )


          ),


          box(width = 6,

            column(width = 2,

              h5(icon("book"), "PCA", width = "100%")

            ),


            column(width = 3,

              selectInput("pcatype",
                label = "Type",
                choices = c("ind", "var", "biplot"),
                selected = "biplot")

            ),

            column(width = 2,

              numericInput("pcaqs",
                label = "Variable",
                value = NA,
                min = 1,
                step = 1
              )

            ),

            column(width = 5,

              textInput("pcalbl",
                label = "Label",
                value =  ""
              )

            )



          ),


          box(width = 6,

            plotOutput("crpt", width = "580px", height = "520px")

          ),

          box(width = 6,


            plotOutput("pca", width = "580px", height = "520px")


          )

        ),


# statistics -------------------------------------------------------------

        tabItem(tabName = "stat",
                
          shiny::fluidRow(


          box(width = 5, background = "black",
              
            column(width = 12,
                   
                   uiOutput("stat_factor")
                   
            ),

            column(width = 6,

              uiOutput("stat_response")

            ),


            column(width = 6,

              uiOutput("stat_block")


            ),


            column(width = 6,

              numericInput("stsig",
                label = "Significance",
                value = 0.05,
                min = 0,
                max = 5,
                step = 0.01)


            ),

            column(width = 6,

              selectInput("stmc",
                label = "Type",
                choices = c("tukey", "duncan", "snk"),
                selected = "snk")


            ),


            column(width = 12,

                verbatimTextOutput("tbav")

            ),
            
            
            column(width = 12,
                   
                   tableOutput("stat_summary")
                   
            )



          ),
          
          
          
          box(width = 7,
              
            
              box(title = "Summary table", 
                  solidHeader = T,
                  width = 12, 
                  collapsible = T,
                  collapsed = T,
                  status = "primary",
                  
                  DT::dataTableOutput("mnc")
                  
                  ),
              
              
              box(title = "Assumptions", 
                  solidHeader = T,
                  width = 12, 
                  collapsible = T,
                  collapsed = T,
                  status = "danger",
                  
    shiny::HTML("<h5><b>Don't forget the assumptions of the model!</b></h5>"),
    
    shiny::HTML("<p>
                  <ol>
                  <li>The errors are independent.</li>
                  <li>The variable should have a normal distribution.</li>
                  <li>The variance should be the same for all treatments.</li>
                  </ol>
                  </p>"),
    
    hr(),
    
    shiny::HTML("<h5><b>The following plots can help you to evaluate the assumptions</b></h5>"),

    column(width = 12,
           
           plotOutput("assuption_plot01")
           
    ),
    
    
    p("Any trend in the residuals would violate the assumption of independence while a trend in the variability of the residuals -for instance a funnel shape- suggests heterogeneity of variances."),
    
    
    column(width = 12,
           
           plotOutput("assuption_plot02")
           
    ),
    
    p("Departures from the theoretical normal line are symptoms of lack of normality.")
  

              )
            
            
          )
          
          
          )

          
        ),

# graphics ----------------------------------------------------------------


        tabItem(tabName = "graph",
                
          
          box( width = 10,


            box(width = 5, title = NULL, background = "blue",


                      column(width = 12,

                        textInput(
                          inputId ="gply",
                          label = "Y label",
                          value = "")


                      ),

                      column(width = 4,


                        numericInput(
                          inputId ="gbrakes",
                          label = "Brakes",
                          value = NA,
                          min = 0
                        )

                      ),


                      column(width = 4,


                        numericInput(
                          inputId ="glmti",
                          label = "Limit (i)",
                          value = NA
                        )

                      ),


                      column(width = 4,


                        numericInput(
                          inputId ="glmtf",
                          label = "Limit (f)",
                          value = NA
                        )

                      )




              ),



            box(width = 4, title = NULL, background = "green",




                  column(width = 12,

                    textInput(inputId ="gplx", label = "X label", value = "")


                  ),


                  column(width = 12,

                    textInput(inputId ="gp_xbk", label = "Brake Text", value = "")


                  )



            ),


            box(width = 3, background = "red",

                  column(width = 12,

                    textInput(inputId ="gplz", label = "Legend", value = "")


                  ),


                column(width = 12,

                  textInput(inputId ="gp_zbk", label = "Brake Text", value = "")


                )

            ),

          shiny::fluidRow(
            
          box(width = 12,


                plotOutput("stplot")



          )


          )
          
      ),

      shiny::fluidRow(
      
          box(width = 2,

            column(width = 12,


              numericInput(
                inputId ="gfont",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1
              )

            ),


            column(width = 12,


              radioButtons(
                inputId ="gtype",
                label = "Type",
                choices = c("bar", "line"),
                selected = "bar",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="gcolor",
                label = "Color",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="gsig",
                label = "Significance",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),

            column(width = 12,


              radioButtons(
                inputId ="gerbr",
                label = "Error",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="glabel",
                label = "Legend",
                choices = c("none", "left", "right", "top", "bottom"),
                selected = "top",
                inline = TRUE)
            ),



            column(width = 12,

              numericInput('plot_H', 'Height (mm)',
                value = 75,
                min = 0,
                step = 5)

             ),


            column(width = 12,


              numericInput('plot_W', 'Width (mm)',
                value = 105,
                min = 0,
                step = 5)


            ),


            column(width = 12,

              downloadButton('download_plot', ' TIFF (300 dpi)')

            )





          )
          
          
          )

        ),


# tools -------------------------------------------------------------------

        tabItem(tabName = "tools", withMathJax(),
                
                
                shiny::fluidRow(
                  
                box(width = 8, 
                    title = "Osmotic potencial calculator", 
                    status = "primary",
                    solidHeader = T,
                    
                    br(),
                    
                    p("The osmotic potencial, can be measured directly with an osmometer, or it can be calculated from the solute concentration."),
                    
                    hr(),
                    
                    p("For a salt, you can use the van't Hoff relation: $$\\psi_s = -RTC_i$$ where: \\(R\\) is the gas constant (i.e. \\(0.0083 L/atm/mol/K\\)), \\(T\\) is the absolute temperature in degrees in Kelvin (\\(273.15^{o}C\\)), 
                      \\(C\\) is the solute concentration in \\(mol*L^{-1}\\), and \\(i\\) is the dissociation constant of the salt. (i.e. \\(NaCl = 1.8, KCl = 1.8, CaCl_2 = 2.4, sacarose = 1\\)). The unit for \\(\\psi_s\\) is \\(MPa\\)"), 
                    
                    hr(),
                    
                    p("For", em("PEG 6000"), "the osmotic potentials can be calculated as described by", a("Michel and Kaufmann (1973):", href = "http://www.plantphysiol.org/content/51/5/914.abstract", target="_blank"), "$$\\psi_s = -(1.18*10^{-2})C - (1.18*10^{-4})C^2 + (2.67*10^{-4})CT + (8.39*10^{-7})C^2T$$ 
                      where: \\(C\\) is the concentration of", em("PEG 6000"), "in \\(g*L^{-1}\\) and \\(T\\) is the temperature in degrees \\(^{o}C\\).  The unit for \\(\\psi_s\\) is \\(bar (0.1 MPa)\\)."),
                    
                    br()
                    
                    ),
                


                     box(
                        title = "Salt (\\(g\\))", 
                        status = "info", 
                        solidHeader = T,
                        width = 2,
                          

                        h3(textOutput("ops")),
                        
                        hr(),
                        
                        numericInput("vol", label = p("Volume (\\(L\\))"), value = 1.0, min = 0),
                        
                        numericInput("pre", label = p("Pressure (\\(MPa\\))"), value = -0.8, max = 0),
                        
                        numericInput("tem", label = p("Temperature (\\(^{o}C\\))"), value = 25.0),
                        
                        numericInput("psm", label = p("Molecular weight"), value = 58.4428, min = 0),
                        
                        numericInput("dis", label = p("Salt dissociation constant"), value = 1.8, min = 0)
                        
                          
                          ),
                      
                  box(
                        title = "PEG 600 (\\(g\\))", 
                        solidHeader = T,
                        status = "info",
                        width = 2,
                          

                        h3(textOutput("opp")),
                        
                        hr(),
                        
                        numericInput("volp", label = p("Volume (\\(L\\))"), value = 1.0, min = 0),
                        
                        numericInput("prep", label = p("Pressure (\\(MPa\\))"), value = -0.8, max = 0),
                        
                        numericInput("temp", label = p("Temperature (\\(^{o}C\\))"), value = 25.0)
                        
                        
                          )
                
                )
                
                
          ),


# Lineal Regression -------------------------------------------------------

        tabItem(tabName = "regression",

          box( width = 10,


            box(width = 4, title = NULL, background = "blue",


              column(width = 12,

                uiOutput("lrg_variable2")


              ),

              column(width = 8,

                textInput("lr_lbv2", label = "Label", value = "")


              ),


              column(width = 4,

                numericInput("lr_brk2", label = "Brakes", value = NA, min = 0)


              )


            ),



            box(width = 4, title = NULL, background = "green",




              column(width = 12,

                uiOutput("lrg_variable1")


              ),

              column(width = 8,

                textInput("lr_lbv1", label = "Label", value = "")


              ),

              column(width = 4,

                numericInput("lr_brk1", label = "Brakes", value = NA, min = 0)

              )


            ),


            box(width = 4, background = "red",

              column(width = 12,

                uiOutput("lrg_grouped")


              ),


              column(width = 6,

                textInput("lr_lbgp", label = "Legend", value = "")


              ),


              column(width = 6,

                textInput("lr_lglv", label = "Levels", value = "")


              )


            ),


            box(width = 12,


              plotOutput("plot_regression")



            )


          ),

          box(width = 2,

            column(width = 12,


              numericInput(
                inputId ="lr_font",
                label = "Size",
                value = 2,
                min = 0,
                step = 0.1
              )

            ),


            column(width = 12,


              radioButtons(
                inputId ="lr_color",
                label = "Color",
                choices = c("yes", "no"),
                selected = "yes",
                inline = TRUE)
            ),


            column(width = 12,


              radioButtons(
                inputId ="lr_label",
                label = "Legend",
                choices = c("none", "left", "right", "top", "bottom"),
                selected = "top",
                inline = TRUE)
            ),


            column(width = 12,

               numericInput('lr_eq_x', 'Eq. x',
                            value = NA),

               numericInput('lr_eq_y', 'Eq. y',
                            value = NA)

            ),




            column(width = 12,

              numericInput('lr_plot_H', 'Height (mm)',
                value = 75,
                min = 0,
                step = 5)

            ),


            column(width = 12,


              numericInput('lr_plot_W', 'Width (mm)',
                value = 105,
                min = 0,
                step = 5)


            ),


            column(width = 12,

              downloadButton('download_plot_lr', ' TIFF (300 dpi)')

            )





          )



        ),


# Germination in time -----------------------------------------------------

  tabItem(tabName = "germint",


          box( width = 10,
               
               
               box(width = 5, title = NULL, background = "blue",
                   
                   
                   column(width = 12,
                          
                          textInput(
                            inputId ="git_ly",
                            label = "Y label",
                            value = "Germination ('%')")
                          
                          
                   ),
                   
                   column(width = 4,
                          
                          
                          numericInput(
                            inputId ="git_brakes",
                            label = "Brakes",
                            value = NA,
                            min = 0
                          )
                          
                   ),
                   
                   
                   column(width = 4,
                          
                          
                          numericInput(
                            inputId ="git_lmti",
                            label = "Limit (i)",
                            value = NA
                          )
                          
                   ),
                   
                   
                   column(width = 4,
                          
                          
                          numericInput(
                            inputId ="git_lmtf",
                            label = "Limit (f)",
                            value = NA
                          )
                          
                   )
                   
                   
                   
                   
               ),
               
               
               
               box(width = 4, title = NULL, background = "green",
                   
                   
                   
                   
                   column(width = 12,
                          
                          textInput(inputId ="git_lx", label = "X label", value = "Time")
                          
                          
                   ),
                   
                   
                   column(width = 12,
                          
                          textInput(inputId ="git_xbk", label = "Brake Text", value = "")
                          
                          
                   )
                   
                   
                   
               ),
               
               
               box(width = 3, background = "red",
                   
                   column(width = 12,
                          
                          textInput(inputId ="git_lz", label = "Legend", value = "")
                          
                          
                   ),
                   
                   
                   column(width = 12,
                          
                          textInput(inputId ="git_zbk", label = "Brake Text", value = "")
                          
                          
                   )
                   
               ),
               
               
               shiny::fluidRow(
               
               box(width = 12,
                   status = "info", 
                   solidHeader = T,
                   
                   
                   plotOutput("GerInTime")
                   
                   
                   
               )
               
               
               )
               
               
          ),
          
          shiny::fluidRow(
          
          box(width = 2,
              
              
              column(width = 12,
                     
                     
                     uiOutput('smvar')
                     
              ),
              
              
              column(width = 12,
                     
                     
                     radioButtons(
                       inputId ="git_type",
                       label = "Type",
                       choices = c("percentage", "relative"),
                       selected = "percentage",
                       inline = F)
              ),
              
              column(width = 12,
                     
                     
                     numericInput(
                       inputId ="git_font",
                       label = "Size",
                       value = 2,
                       min = 0,
                       step = 0.1
                     )
                     
              ),
              
              
              column(width = 12,
                     
                     
                     radioButtons(
                       inputId ="git_color",
                       label = "Color",
                       choices = c("yes", "no"),
                       selected = "yes",
                       inline = TRUE)
              ),
              
              

              
              
              column(width = 12,
                     
                     
                     radioButtons(
                       inputId ="git_label",
                       label = "Legend",
                       choices = c("none", "left", "right", "top", "bottom"),
                       selected = "top",
                       inline = TRUE)
              ),
              
              
              column(width = 12,
                     
                     numericInput('git_plot_H', 'Height (mm)',
                                  value = 75,
                                  min = 0,
                                  step = 5)
                     
              ),
              
              
              column(width = 12,
                     
                     
                     numericInput('git_plot_W', 'Width (mm)',
                                  value = 105,
                                  min = 0,
                                  step = 5)
                     
                     
              ),
              
              
              column(width = 12,
                     
                     downloadButton('download_plot_git', ' TIFF (300 dpi)')
                     
              )
              
              
              
          )
          
          
          )


          )


      )



    )

  )
)



