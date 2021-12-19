
source("global.R")

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
                 fluidRow(column(width=6, shinydashboard::box(title = "Step 1 - Load Data", 
                                                              width = NULL,
                                                              status = "primary", 
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              style = 'overflow-y: scroll', 
                                                              uiOutput("get_data_upload_basis_helper")
                 )),
                 
                 column(width=6, shinydashboard::box(title = "Step 2 - Method Comparison", 
                                                     width = NULL,
                                                     status = "warning", 
                                                     solidHeader = TRUE,
                                                     collapsible = TRUE,
                                                     style = "overflow-y: scroll", 
                                                     uiOutput("get_method_comparison_helper"),
                 )
                 )
                 ),
                 
                 fluidRow(column(width=6, shinydashboard::box(title = "Step 3 - Method Scoring", 
                                                              width = NULL,
                                                              status = "success", 
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              style = "overflow-y: scroll", 
                                                              uiOutput("get_method_scoring_helper")             
                 )
                 ),
                 column(width=6, shinydashboard::box(title = "Step 4 - Report", 
                                                     width = NULL,
                                                     status = "info", 
                                                     solidHeader = TRUE,
                                                     collapsible = TRUE,
                                                     style = "overflow-y: scroll", 
                                                     uiOutput("get_report_helper")             
                 )
                 )
                 ),
                 shiny::HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' 
                             style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' 
                             /></a><br />This work is licensed under a <a rel='license' 
                             href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")
),
tabItem(tabName = "data", 
        tabBox(title = "", id = "tab_data_upload", width = 12,
               tabPanel("Data Upload",
                        fluidRow(column(5, box(title = shiny::p("Upload Data", tags$span(icon("info-circle"), 
                                                                id = "icon_performance_2", style = "color: #C0C0C0;")),
                                               radioButtons(
                                                   "data_input_option", label = NULL,
                                                   choices = dist_options
                                                   ,
                                                   selected =  1),
                                               fileInput("upload_file", label = "", multiple = FALSE, 
                                                         accept = c(".csv")),
                                               bsPopover("icon_performance_2",
                                                         title = "What is data demo?",
                                                         content = paste("Three sample data used to compare methods with more than a one test",
                                                                         "there are 3 samples: Blood Presure (3 methods), Plasma & T4 (1 test method)",
                                                                         "or loading a CSV file from personal computer.",
                                                                         sep = "<br>"),
                                                         placement = "right",
                                                         options = list(container = 'body'))
                                               
                                               ),
                                        
                                        
                                        )
                                 
                        ),
                     
                        fluidRow(column(DT::dataTableOutput("raw_data"),
                                        width = 12))
               ),
               tabPanel("Visualization Bland-Altman", 
                        fluidRow(column(width=2, selectInput("method_list_2", label = "Method Test Comparison", choices = c(""))),
                                 column(width=8, plotlyOutput("bland_altman")),
                                 column(width=2, uiOutput("n_points_3")),
                                 )
                        
               )
               )),

tabItem(tabName = "fit",
        box(title = "Method Comparison Parameters", status = "primary", solidHeader = TRUE, width = 12,
            collapsible = T,collapsed = F,
            fluidRow(column(width=2, selectInput("method_list", label = "Method Test Comparison", choices = c(""))),
                     column(width=2, uiOutput("n_points")))),
        tabBox(title = "", id = "tab_fit", width = 12,
               tabPanel("Summary",
                        fluidRow(column(width=2)),
                        fluidRow(column(width=10, plotlyOutput("all_regressions")))),
               tabPanel("Graphical evaluation",
                        fluidRow(column(width=4, plotOutput("ols_reg")),
                                 column(width=4, plotOutput("olsw_reg")),
                                 column(width=4, plotOutput("deming_reg"))),
                        fluidRow(column(width=4, plotOutput("demingw_reg")),
                                 column(width=4, plotOutput("PB_reg")))),
               tabPanel("Regression Parameters", 
                        fluidRow(column(width=6, DT::dataTableOutput("summary_stats")))
                        ),
               tabPanel("Comparability",
                        shiny::p("A hypothesis test to compare the value of the means of the reference method vs the test method is carried out to establish whether under this test the methods can be considered interchangeable.",
                                 style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                 uiOutput("results_test_hypothesis"))
            )
       ),
tabItem(tabName = "result",
        tabBox(title = "", id = "tab_result", width = 12,
               tabPanel("Ranking Methods",
                        fluidRow(column(width=5, uiOutput("n_points_2"))),
                        fluidRow(column(width=5, DT::dataTableOutput("summary_ranking")))
               )
            )
        ),
tabItem(tabName = "report",
        tabBox(title = "", id = "tab_report", width = 12,
               tabPanel("Report",
                        shiny::p("There are two format options (PDF-HTML) in order to get the document that includes all-in-one detailed analysis report,
                                 about what test method is better to replace reference method. It takes some seconds to produce the report.",
                                 style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                        radioButtons("format", "Select document format:",
                                     choiceNames = list(
                                         shiny::p(icon("html5"), "HTML"),
                                         shiny::p(icon("file-pdf"), "PDF")
                                     ),
                                     choiceValues = list(
                                         "HTML", "PDF"
                                     ),
                                     inline = TRUE),
                        downloadButton('downloadReport')
                       )
               )
        ),

tabItem(tabName = "about",
        
        h3('About method comparison'),
        shiny::p("In the clinical measurement of in-vitro tests, comparison of a new measurement technique against an established one 
                 to see if they match enough as so that the new one replaces the previous one and it is possible to have a sustenance 
                 to affirm that the methods are interchangeable, a guide that standardizes and proposes good practices for these studies 
                 is: CLSI-EP09-A3."),
        shiny::p(tags$a(href="https://clsi.org/standards/products/method-evaluation/documents/ep09/",
                        icon("file-pdf"), 
                        target="_blank", "CLSI. Measurement Procedure Comparison and Bias Estimation Using Patient Samples, 
                 Approved Guideline-Third Edition. CLSI document EP09-A3. Wayne, PA: Clinical and Laboratory Standards Institute; 2013." 
        )),
        
        h3('Contributors'),
        shiny::p('This solution was created by'),
        tags$ul(
            tags$li(tags$a(href="https://www.linkedin.com/in/javier-samir-rey-7104195/", 
                           icon="github", target="_blank",
                           "Javier Rey"))
        ),
        
        h3('Related master TFM'),
        shiny::p("Web app for compare methods.",
          br(),
          "Rey R Javier",
          br(),
          "UoC",
          tags$a(href="https://doi.org/10.1101/xxx",
                 icon("file-pdf"), target="_blank", 
                 "")),
        
        h3("Related work"),
        shiny::p("This web application multi method comparison draws inspiration from:"),
        shiny::p(tags$a(href="https://bahar.shinyapps.io/method_compare/",
                        icon("html5"), 
                        target="_blank", "Method Comparison and Bias Estimation Using R and Shiny." 
        )),
        
        h3('Source code'),
        shiny::p('The source code V.1 for this application can be found at ', tags$a(href="https://jasam.shinyapps.io/multi_method_comparison/", icon("github"), target="_blank", "https://jasam.shinyapps.io/multi_method_comparison/")),
        shiny::p('Bug reports and feature requests can be submitted through the GitHub issues page ',  
                 tags$a(href="https://github.com/jasam/multi_method_comparison/issues", icon("github"), 
                        target="_blank", "https://github.com/jasam/multi_method_comparison/issues")),
        
        h3('Packages used'),
        shiny::p('1. data.table: Extension of `data.frame`', tags$a(href="https://CRAN.R-project.org/package=data.table/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://CRAN.R-project.org/package=data.table")),
        
        shiny::p('2. dplyr: A Grammar of Data Manipulation', tags$a(href="https://CRAN.R-project.org/package=dplyr/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://CRAN.R-project.org/package=dplyr")),
        
        shiny::p('3. DT: A Wrapper of the JavaScript Library DataTables', tags$a(href="https://github.com/rstudio/DT/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://github.com/rstudio/DT")),
        
        shiny::p('4. mcr: Method Comparison Regression', tags$a(href="https://CRAN.R-project.org/package=mcr/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://CRAN.R-project.org/package=mcr")),
        
        shiny::p('5. plotly: Interactive Web-Based Data Visualization with R, plotly, and shiny', tags$a(href="https://plotly-r.com/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://plotly-r.com")),
        
        shiny::p('6. purrr: Functional Programming Tools', tags$a(href="https://CRAN.R-project.org/package=purrr/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://CRAN.R-project.org/package=purrr")),
        
        shiny::p('7. rmarkdown: Dynamic Documents for R', tags$a(href="https://CRAN.R-project.org/package=rmarkdown/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://CRAN.R-project.org/package=rmarkdown")),
        
        shiny::p('8. shiny: Web Application Framework for R', tags$a(href="https://shiny.rstudio.com/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://shiny.rstudio.com/")),
        
        shiny::p('9. shinyBS: Twitter Bootstrap Components for Shiny', tags$a(href="https://ebailey78.github.io/shinyBS/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://ebailey78.github.io/shinyBS/")),
        
        shiny::p('10. shinydashboard: Create Dashboards with Shiny', tags$a(href="http://rstudio.github.io/shinydashboard/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "http://rstudio.github.io/shinydashboard/")),
        
        shiny::p('11. tinytex: Helper Functions to Install and Maintain TeX Live, and Compile LaTeX Documents', 
                                                                tags$a(href="https://github.com/yihui/tinytex/", 
                                                                 icon("file-pdf"), 
                                                                 target="_blank", "https://github.com/yihui/tinytex/")),
    
        shiny::p('12. pander: An R Pandoc Writer', 
                 tags$a(href="https://rapporter.github.io/pander/", 
                 icon("file-pdf"), 
                 target="_blank", "https://rapporter.github.io/pander/")),
    
        h3("Acknowledgements"),
        shiny::p("This development was supported by the generous guidance and supervision of Susana Perez Alvarez, 
                 I received a lot of helpful reviews from her knowledge. She is master's Data Science professor at Univesitat Oberta of Catalunya."),
        
        shiny::HTML("<a rel='license' href='http://creativecommons.org/licenses/by-nc/4.0/'><img alt='Creative Commons Licence' 
                     style='border-width:0' src='https://i.creativecommons.org/l/by-nc/4.0/88x31.png' 
                     /></a><br />This work is licensed under a <a rel='license' 
                     href='http://creativecommons.org/licenses/by-nc/4.0/'>Creative Commons Attribution-NonCommercial 4.0 International License</a>.")
                   )
    
 )

)
## Deploy page
dashboardPage(header, sidebar, body)