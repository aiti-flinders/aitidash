#Module UI Function
#Labour Market tab/tabPanel


labourMarketUI <- function(id, data) {
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
  
  min_date <- min(data$year)
  max_date <- max(data$year)
  
  
  
  
  tabPanel(title = uiOutput(ns("title_panel")),
           plotlyOutput(ns("plot"), width = '100%'),
           fluidRow(
             box(width = 4, status= 'info',solidHeader=TRUE,
                 selectInput(
                   inputId = ns('indicator'), 
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Unemployment rate")
                 ),
             box(width = 4, status = "info", solidHeader = TRUE,
                 selectInput(
                   inputId = ns('series_type'),
                   label =  "Select Series Type", 
                   choices = series_choices,
                   selected = "Seasonally Adjusted")
                 ),
             box(width = 4, status = "info", solidHeader = TRUE,
                 numericInput(
                   inputId = ns('years'),
                   label = 'Select Year',
                   value = 2015,
                   min = min_date,
                   max = max_date)
                 )),
             fluidRow(
             box(width = 12, title = "Download what you see", solidHeader = F,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Download Plot",
                   class = 'download-button'
                   ),
                 downloadButton(
                   outputId = ns("download_data"),
                   label = "Download Data",
                   class = 'download-button'
                   ))
           )
           
  )
}



labourMarket <- function(input, output, session, data, region) {
  

  output$title_panel = renderText({
    region()
  })
  
  current_selection <- reactiveVal(NULL)
  
  observeEvent(input$tabs, {
    
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(indicator == input$indicator,
                               state == region()) %>%
                        pull(series_type) %>%
                        unique())
  })
  
  observeEvent(region(), {
    
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(indicator == input$indicator,
                               state == region()) %>%
                        pull(series_type) %>%
                        unique())
  })
  
  create_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             year >= input$years,
             state == region(),
             age == "Total (age)",
             gender == "Persons",
             series_type == input$series_type) %>%
      select(date, indicator, value, gender, age, state)
    
  })
  
  create_plot <- reactive({
    p <- abs_plot(indicator = input$indicator,
                  years = input$years,
                  state = region(),
                  series_type = input$series_type,
                  compare_aus = FALSE,
                  plotly = TRUE) 
  })
  

  output$plot <- renderPlotly({
    
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

