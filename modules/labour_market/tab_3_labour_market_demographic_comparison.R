##Demographic Module


labourMarketDemogUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Unemployed total",
                         "Underemployed total",
                         "Underutilised total",
                         "Unemployment rate",
                         "Underemployment rate (proportion of labour force)",
                         "Underutilisation rate",
                         "Participation rate")
  
  series_choices <- data %>%
    pull(series_type) %>%
    unique() %>%
    sort()
  

  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Demography", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status='info', solidHeader=TRUE,
                 selectInput(
                   inputId = ns('indicator'),
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Unemployment rate"),
                 selectInput(
                   inputId = ns("series_type"),
                   label = "Select Series Type",
                   choices = series_choices,
                   selected = "Seasonally Adjusted")),
             box(status = 'info', solidHeader = TRUE,
                 numericInput(
                   inputId = ns("years"),
                   label = 'Select Number of Years',
                   value = 3,
                   min = 1,
                   max = 42,
                   step = 1),
                 selectInput(
                   inputId = ns("demographic"),
                   label = "Select Demographic Variable",
                   choices = c("Age", "Gender")
                 ))
           )
  )
}

labourMarketDemog <- function(input, output, session, data, region) {
  
  genders <- reactive({
    if(input$demographic == "Gender") {
      genders <- c("Males", "Females")
    } else {genders <- "Persons"}
  })
  
  ages <- reactive({
    if(input$demographic == "Age" & region() == "Australia") {
      ages <- c("15-24 years",
                "25-34 years",
                "35-44 years",
                "45-54 years",
                "55 years and over")
    } else {ages <- "Total (age)"}
  })
  
  choices <- reactive({
    if(region() == "Australia") {
      choices <- c("Age", "Gender")
    } else {choices <- "Gender"}
  })
  
  observeEvent(region(), {
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(indicator == input$indicator,
                               state == region()) %>%
                        pull(series_type) %>%
                        unique() %>%
                        sort())
    updateSelectInput(session, "demographic", 
                      choices = choices())
  })
  

  output$plot <- renderPlotly({
    
    abs_plot(indicator = input$indicator,
               states = region(),
               series_type = input$series_type,
               genders = genders(),
               ages = ages(),
               years = input$years,
               compare_aus = FALSE,
               plotly = TRUE)
  })
}