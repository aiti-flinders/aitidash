

labourMarketUI <- function(id, data) {

  min_date <- min(data$year)
  max_date <- max(data$year)
  
  tabPanel(title = "Regional Comparison", 
           plotlyOutput(NS(id, "plot"), width = '100%', height = "400px"),
           fluidRow(
             dashboard_box(title = "Customise Chart", 
                           selectInput(
                             inputId = NS(id, "indicator"), 
                             label = "Select Indicator",
                             choices = labour_market_indicators()
                           ),
                           radioGroupButtons(
                             inputId = NS(id, 'series_type'),
                             label =  "Select Series Type", 
                             choices = series_choices(),
                             selected = "Seasonally Adjusted", 
                             direction = 'horizontal'
                           ),
                           numericInput(
                             inputId = NS(id, 'years'),
                             label = 'Select Start Year',
                             value = max_date - 5,
                             min = min_date,
                             max = max_date)
             ),
             dashboard_box(title = "Add Regions",
                           checkboxGroupButtons(
                             inputId = NS(id, "state"),
                             label = NULL,
                             choices = regions(),
                             selected = "Australia",
                             direction = "vertical",
                             justified = TRUE
                           )
                           
             ),
             box(title = "Downloads", width = 4, status = "primary", solidHeader = FALSE, headerBorder = TRUE, collapsible = FALSE,
                 download_graph_ui(id),
                 uiOutput(inline = TRUE, NS(id, "download_report_button"))
             )
           )
  )
  
}


labourMarketServer <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
    

  
  current_indicator <- reactiveVal(NULL)
  current_regions <- reactiveVal(NULL)
  current_series_type <- reactiveVal(NULL)
  
  observeEvent(input$indicator, {
    current_indicator(input$indicator)
    current_series_type(input$series_type)
    updateRadioGroupButtons(session,
                            "series_type",
                            choices = data %>%
                              filter(state %in% input$state,
                                     indicator == input$indicator) %>%
                              distinct(series_type) %>%
                              pull() %>%
                              sort(),
                            selected = current_series_type()
    )
  })
  
  observeEvent(input$series_type, {
    current_indicator(input$indicator)
    updateSelectInput(session, 
                      "indicator", 
                      choices = data %>%
                        filter(state %in% input$state,
                               series_type == input$series_type,
                               indicator %in% labour_market_indicators()) %>%
                        distinct(indicator) %>%
                        pull() %>%
                        sort(),
                      selected = current_indicator()
    )
  })
  

  create_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             year >= input$years,
             state %in% input$state,
             age == "Total (age)",
             gender == "Persons",
             series_type == input$series_type) %>%
      select(date, indicator, value, gender, age, state)
    
  })
  
  create_plot <- reactive({
    p <- abs_plot(indicators = input$indicator,
                  years = input$years,
                  states = input$state,
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
      paste0(input$filename, "-plot.", input$filetype)
    },
    content = function(file) {
      plotly_IMAGE(create_plot(), format = input$filetype, width = input$width, height = input$height, out_file = file)
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
  

  
  report_url <- reactive({
    paste0("https://www.flinders.edu.au/content/dam/documents/research/aiti/monthly-employment-insights/",
                      tolower(gsub(x = input$state, pattern = " ", replacement = "-")),
                      ".pdf")
  })
  
  output$download_report_button <- renderUI({
    if(all(input$state != "Australia") & length(input$state) == 1)
    downloadButton(
      outputId = session$ns("download_report"),
      label = paste("Download report for ", input$state),
      class = "download-button"
    )
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("AITI Labour Market Brief - ", input$state, ".pdf")
    },
    content = function(file) {
      download.file(report_url(), file, mode = "wb")
    }
  )
    }
  )
}




