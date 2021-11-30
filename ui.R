
# Header page
header = dashboardHeader(title = "Method Comparison",
                         tags$li(class="dropdown",
                                 tags$a(href="https://github.com/jasam/multi_method_comparison", 
                                        icon("github"), "Source Code", target="_blank")))

# Side Menu
sidebar = dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("question-circle-o")),
    menuItem("Data", tabName = "data", icon = icon("folder-open-o")),           
    menuItem("Method Comparison", tabName = "fit", icon = icon("clone")),           
    menuItem("Methods Scoring", tabName = "result", icon = icon("exchange")),           
    menuItem("Report", tabName = "report", icon = icon("pie-chart")),           
    menuItem("About", tabName = "about", icon = icon("info-circle fa-lg"))           
))
# Body Page
body = dashboardBody(
    
tabItems(tabItem(tabName = "intro",
                 h2("Welcome to method comparison solution"),
                 fluidRow(column(width=6, box(title = "Step 1 - Load Data", 
                                              width = NULL,
                                              status = "primary", 
                                              solidHeader = TRUE,
                                              collapsible = TRUE,
                                              style = 'overflow-y: scroll', 
                                              uiOutput("get_data_upload_basis_helper")
                 )),
                 
                 column(width=6, box(title = "Step 2 - Method Comparison", 
                                     width = NULL,
                                     status = "primary", 
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     style = "overflow-y: scroll", 
                                     uiOutput("get_method_comparison_helper")
                 )
                 )
                 ),
                 
                 fluidRow(column(width=6, box(title = "Step 3 - Method Scoring", 
                                              width = NULL,
                                              status = "primary", 
                                              solidHeader = TRUE,
                                              collapsible = TRUE,
                                              style = "overflow-y: scroll", 
                                              uiOutput("get_method_scoring_helper")             
                 )
                 ),
                 column(width=6, box(title = "Step 4 - Report", 
                                     width = NULL,
                                     status = "primary", 
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     style = "overflow-y: scroll", 
                                     uiOutput("get_report_helper")             
                 )
                 )
                 )
                 # shiny::HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' 
                 #      style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' 
                 #      /></a><br />This work is licensed under a <a rel='license' 
                 #      href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")
),
tabItem(tabName = "data", 
        tabBox(title = "", id = "tab_data_upload", width = 12,
               tabPanel("Data Upload",
                        fluidRow(column(5, box(title = shiny::p("Upload Data", tags$span(icon("info-circle"), 
                                                                id = "icon_performance_2", style = "color: #C0C0C0;")),
                                               radioButtons(
                                                   "data_input_option", "",
                                                   choices = 
                                                       list("Example Blood Pressure" = 1,
                                                            "Example Plamas" = 2,
                                                            "Example T4" = 3,
                                                            "Upload file (CSV)" = 4
                                                       )
                                                   ,
                                                   selected =  1),
                                               fileInput("upload_file", label = "", multiple = FALSE, 
                                                         accept = c(".csv")),
                                               bsPopover("icon_performance_2",
                                                         title = "What is data demo?",
                                                         content = paste("Below you can find one of three sample data used to compare methods with more than a one test",
                                                                         "there are 3 samples: Blood Presure (3 methods), Plasma & T4 (1 test method)",
                                                                         sep = "<br>"),
                                                         placement = "right",
                                                         options = list(container = 'body'))
                                               
                                               ),
                                        
                                        
                                        )
                                 
                        ),
                     
                        fluidRow(column(DT::dataTableOutput("raw_data"),
                                        width = 12))
               ),
               tabPanel("Bland-Altman", 
                        fluidRow(column(width=2),
                                 column(width=8, plotlyOutput("bland_altman")))
                        
               )
               )),

tabItem(tabName = "fit",
        # p("Configuracion de parametros, como escogencia del método de test (si hay mas de uno para comparar contra el método de referencia,
        #   Bland-Altman plot, intervalo de confianza, unidades, puntos de decisión médica, rangos, etc",
        #   style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
        box(title = "Method Comparison Parameters", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = T,collapsed = F,
            fluidRow(column(2, selectInput("method_list", label = "Method Test Comparison", choices = c(""))))),
        tabBox(title = "", id = "tab_fit", width = 12,
               tabPanel("Summary",
                        fluidRow(column(width=2),
                                 column(width=8, plotlyOutput("all_regressions")))),
               tabPanel("Graphical evaluation",
                        #h4("Data Points:"),
                        #textOutput("n_points"),
                        fluidRow(column(width=4, plotOutput("ols_reg")),
                                 column(width=4, plotOutput("olsw_reg")),
                                 column(width=4, plotOutput("deming_reg"))),
                        fluidRow(column(width=4, plotOutput("demingw_reg")),
                                 column(width=4, plotOutput("PB_reg")))),
               tabPanel("Statistics", 
                        fluidRow(column(width=6, DT::dataTableOutput("summary_stats")))
                        ),
               tabPanel("Comparability",
                        uiOutput("results_test_hypothesis"))
            )
       ),
tabItem(tabName = "result",
        tabBox(title = "", id = "tab_result", width = 12,
               tabPanel("Ranking Methods",
                        # p("Clasificacion de los métodos de acuerdo a un raking teniendo encuenta los tipos de regressión, se prioriza
                        #   por la mejor pendiente",
                        #   style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
                        # ),
                        fluidRow(column(width=5, DT::dataTableOutput("summary_ranking")))
               )
            )
        ),
tabItem(tabName = "report",
        tabBox(title = "", id = "tab_report", width = 12,
               tabPanel("Report",
                        # p("Opción para visualizar y descargar los resultados en PDF, incluira descripción de los métodos empleados, 
                        #   consideraciones de la norma EP09A3 relevantes, el ranking, los valores de las estimaciones, parámetros relevantes, etc",
                        #   style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                        
                        radioButtons("format", h5('Document format'), 
                                     c('PDF', 'HTML'),
                                     inline = TRUE),
                        downloadButton('downloadReport')
                       )
               )
        ),

tabItem(tabName = "about",
        verbatimTextOutput("app_summary"),
        h3('Contributors'),
        shiny::p('This solution was created by'),
        tags$ul(
            tags$li(tags$a(href="https://www.linkedin.com/in/javier-samir-rey-7104195/", 
                           icon="github", target="_blank",
                           "Javier Rey"))
        ),
        
        h3('About method comparison'),
        shiny::p("In the clinical measurement of in-vitro tests, comparison of a new measurement technique against an established one 
                 to see if they match enough as so that the new one replaces the previous one and it is possible to have a sustenance 
                 to affirm that the methods are interchangeable, a guide that standardizes and proposes good practices for these studies 
                 is: CLSI-EP09-A3."),
        shiny::p(tags$a(href="https://clsi.org/standards/products/method-evaluation/documents/ep09/",
                 icon("file-pdf"), target="_blank", "CLSI. Measurement Procedure Comparison and Bias Estimation Using Patient Samples, 
                 Approved Guideline-Third Edition. CLSI document EP09-A3. Wayne, PA: Clinical and Laboratory Standards Institute; 2013." 
                )),
        
        hr(),
        
        h3('Related master TFM'),
        shiny::p("Web app for compare methods.",
          br(),
          "Rey R Javier",
          br(),
          "UoC 2021.12.19.21255067; doi: ",
          tags$a(href="https://doi.org/10.1101/xxx",
                 icon("file-pdf"), target="_blank", 
                 "https://doi.org/10.1101/xxx")),
        
        hr(),
        
        h3('Source code'),
        shiny::p('The source code for this application can be found at ', tags$a(href="https://jasam.shinyapps.io/multi_method_comparison/", icon("github"), target="_blank", "https://jasam.shinyapps.io/multi_method_comparison/")),
        shiny::p('Bug reports and feature requests can be submitted through the GitHub issues page ',  tags$a(href="https://github.com/jasam/multi_method_comparison/issues", icon("github"), target="_blank", "https://github.com/jasam/multi_method_comparison/issues")),
        
        hr(),
        
        h3("Acknowledgements"),
        shiny::p("This development was supported by the generous guidance and supervision of Susana Perez Alvarez, professor of Univesitat Oberta de Catalunya.")
        
        # HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' 
        #      style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' 
        #      /></a><br />This work is licensed under a <a rel='license' 
        #      href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")
        # )
         )
 )

)
## Deploy page
dashboardPage(header, sidebar, body)        

#ververbatimTextOutput("lr_summary")
# box(title = "Testing", status = "primary", solidHeader = TRUE, width = 12,
#     plotOutput("distPlot"),collapsible = T,collapsed = T)