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

  date_min <- min(data$year)
  date_max <- max(data$year)
  
  tabPanel(title = "Regional Comparison", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status= 'info', solidHeader = FALSE,
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
                   label = 'Select Start Year',
                   value = 2015,
                   min = date_min,
                   max = date_max)),
             box(status='info', solidHeader = FALSE,
                 checkboxGroupInput(
                   inputId = ns('state'),
                   label = "Select Comparison Region",
                   choices = state_choices))
             ),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = F,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'
                 ),
                 downloadButton(
                   outputId = ns("download_data"),
                   label = "Click here to download the chart data",
                   class = 'download-button'
                 ))
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

  
  create_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             year >= input$years,
             state %in% c(region(), input$state),
             age == "Total (age)",
             gender == "Persons",
             series_type == input$series_type) %>%
      mutate(state = factor(state, levels = c(region(), input$state))) %>%
      select(date, indicator, value, gender, age, state)
  })
  
  create_plot <- reactive({
    p <- abs_plot(indicator = input$indicator, 
             years = input$years,
             states = c(region(), input$state),
             series_type = input$series_type,
             compare_aus = FALSE,
             plotly = TRUE)
  })
  
  output$plot <- renderPlotly({
    validate(
      need(nrow(create_data()) > 0, message = FALSE)
      )
    create_plot()
    })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste(input$indicator, "-plot.png", sep = '')
    },
    content = function(file) {
      plotly_IMAGE(create_plot(), out_file = file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$indicator, "-data.csv", sep = '')
    },
    content = function(file) {
      write.csv(create_data(), file, row.names = FALSE)
    }
  )
}