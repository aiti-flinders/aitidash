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
                   value = 2015,
                   min = min_date,
                   max = max_date
                   )),
             box(status = "info", solidHeader = TRUE,
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
  
  plot_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             year >= max(.$year) - input$years,
             state == region(),
             age == "Total (age)",
             gender == "Persons",
             series_type == input$series_type) %>%
      select(date, indicator, value, gender, age, state)
    
  })
  

  output$plot <- renderPlotly({
    
    p <- abs_plot(indicator = input$indicator,
                  years = input$years,
                  state = region(),
                  series_type = input$series_type,
                  compare_aus = FALSE,
                  plotly = TRUE) 
    
    plotly_IMAGE(p, out_file = paste(input$indicator,"-", region(), ".png", sep = ''))
    
    p
    
    
      })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste(input$indicator, "-", region(), ".png", sep = '')
      },
    content = function(file) {
      file.copy(paste(input$indicator,"-", region(), ".png", sep = ''), file, overwrite = TRUE)}
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$indicator, "-data.csv", sep = '')
    },
    content = function(file) {
      write.csv(plot_data(), file, row.names = FALSE)
    }
  )
  


}

