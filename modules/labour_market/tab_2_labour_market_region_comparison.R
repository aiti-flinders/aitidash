  #Regional Comparisons


labourMarketRegionalUI <- function(id, data) {
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
  
  series_choices <- sort(unique(data$series_type))
  state_choices <- sort(unique(data$state))

  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Regional Comparison", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status= 'info',solidHeader=TRUE,
                 selectInput(
                   inputId = ns('indicator'), 
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Unemployment rate"),
                 selectInput(
                   inputId = ns('series_type'),
                   label =  "Select Series Type", 
                   choices = series_choices,
                   selected = "Seasonally Adjusted"),
                 numericInput(
                   inputId = ns('years'),
                   label = 'Select Years',
                   value = 3,
                   min = 1,
                   max = 42,
                   step = 1)),
             box(status='info',solidHeader=TRUE,
                 checkboxGroupInput(
                   inputId = ns('state'),
                   label = "Select Comparison Region",
                   choices = state_choices))
             )
           )
} 

labourMarketRegional <- function(input, output, session, data, region) {
  
  current_selection <- reactiveVal(NULL)
  
  observeEvent(region(), {
    
    updateCheckboxGroupInput(session, "state", choices = data %>%
                               filter(series_type == input$series_type,
                                      state != region()) %>%
                               pull(state) %>%
                               unique() %>%
                               sort())
    
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(indicator == input$indicator,
                               state == region()) %>%
                        pull(series_type) %>%
                        unique() %>%
                        sort())
  })


  observeEvent(input$series_type, {
    
    updateCheckboxGroupInput(session, "state", choices = data %>%
                               filter(series_type == input$series_type,
                                      state != region()) %>%
                               pull(state) %>%
                               unique() %>%
                               sort())
  })

  
  output$plot <- renderPlotly({
    
    validate(
      need(nrow(data[data$indicator == input$indicator & data$state == region(), ]), "Updating")
    )
    
    abs_plot(indicator = input$indicator, 
             years = input$years,
             states = c(region(), input$state),
             series_type = input$series_type,
             compare_aus = FALSE,
             plotly = TRUE)
    

    

  })
}