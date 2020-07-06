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
  

  date_min <- min(data$year)
  date_max <- max(data$year)
  
  tabPanel(title = "Demography", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status='info', solidHeader = FALSE,
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
             box(status = 'info', solidHeader = FALSE,
                 numericInput(
                   inputId = ns("years"),
                   label = 'Select Year',
                   value = 2015,
                   min = date_min,
                   max = date_max),
                 selectInput(
                   inputId = ns("demographic"),
                   label = "Select Demographic Variable",
                   choices = c("Age", "Gender")
                 )),
             fluidRow(
               box(width = 12, title = "Downloads", status = "info", solidHeader = FALSE,
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
  
  create_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             state == region(),
             series_type == input$series_type,
             gender == genders(),
             age == ages(),
             year >= input$years)
  })
  
  create_plot <- reactive({
    p <- abs_plot(indicator = input$indicator,
               states = region(),
               series_type = input$series_type,
               genders = genders(),
               ages = ages(),
               years = input$years,
               compare_aus = FALSE,
               plotly = TRUE)
  })
  

  output$plot <- renderPlotly({create_plot()})
  
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