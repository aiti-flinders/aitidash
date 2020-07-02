##inputs module

setupInput <- function(id, data) {
  ns <- NS(id)
  ns <- NS(id)
  
  indicator_choices <- data %>%
    pull(type) %>%
    unique() %>%
    sort()
  
  state_choices <- data %>%
    pull(state) %>% 
    unique() %>%
    sort() 
  
  series_choices <- data %>% 
    pull(series_type) %>%
    unique() %>%
    sort()
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "A", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info', solidHeader = TRUE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Unemployment Rate"),
                 selectInput(
                   inputId = ns("series_type"),
                   label = "Select Series Type",
                   choices = series_choices,
                   selected = "Trend"),
                 sliderInput(
                   inputId = ns('date_range'),
                   label = "Select Date Range",
                   min = date_min,
                   max = date_max, 
                   value = c(date_min, date_max),
                   timeFormat = "%Y-%b")),
             box(status = 'info', solidHeader = TRUE,
                 radioButtons(
                   inputId = ns("state"),
                   label = "Select Region",
                   choices = state_choices,
                   selected = "Australia"))
           )
  )
}

setup <- function(input, output, session) {
  states <- reactive({input$state})
  
  return(states)
}