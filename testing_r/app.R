library(shiny)
library(shinyBS)

ui <- fluidPage(
    
    tags$head(tags$style(HTML("#button1_div .tooltip {width: 300px;}"))),
    
    
    
    actionButton(inputId = "button2",
                 label = "Second"),
    bsTooltip(id = "button2",
              title = "Hello!",
              placement = "right",
              trigger = "hover")
    
)

server <- function(input, output, session) {
    
}

shinyApp(ui, server)
