#Dashboard Page Module

dashboardUI <- function(id) {
  
  ns <- NS(id)
  
  region_choices = c("Australia", "South Australia")
  
  fluidRow(
    box(status = 'info', solidHeader = TRUE,
        selectInput(label = 'Select Region',
                    inputId = ns("state"),
                    choices = region_choices
        )))
  
}

setup <- function(input, output, session) {
  states <- reactive({input$state})
  
  return(states)
}
  