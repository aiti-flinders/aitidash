industryAustraliaUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- c("Gross domestic product",
                         "GDP per capita")
  
  series_types <- data %>%
    select(series_type) %>%
    pull() %>%
    unique()
  
  measure_choices <- c("Chain volume measures", "Current prices", "Index")
  
  tabPanel(title = "Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 selectInput(inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = indicator_choices
                 ),
                 selectInput(inputId = ns("measure"),
                             label = "Select Measure",
                             choices = measure_choices)),
             box(status = 'info', solidHeader = TRUE,
                 checkboxGroupInput(inputId = ns('series_type'),
                              label = "Select Series Type",
                              choices = series_types,
                              selected = 'Trend'))),
           fluidRow(
             box(status = 'info', solidHeader = TRUE,
                 radioButtons(inputId = ns("quarterly"),
                             label = "Growth Period",
                             choices = c("Annual", "Quarter"))),
             box(status = 'info', solidHeader = TRUE,
                 sliderInput(inputId = ns('date'),
                             label = 'Select Date Range',
                             min = min(data$date),
                             max = max(data$date), 
                             value = c(min(data$date), max(data$date)),
                             timeFormat = "%Y-%b"))
           ))
                              
  
}

industryAustralia <- function(input, output, session, data) {
  
  
  
  observe({
    if(length(input$series_type) < 1) {
      updateCheckboxGroupInput(session, "series_type", selected = "Trend")
    }
    if(!exists('df')) {
      updateCheckboxGroupInput(session, 'series_type', selected = 'Seasonally Adjusted')
    }
  })
  
  current_selection <- reactiveVal(NULL)

  observeEvent(input$indicator, {
    current_selection(input$measure)

    updateSelectInput(session, "measure", choices = data %>%
                        filter(type == input$indicator) %>%
                        pull(measure) %>%
                        unique() %>%
                        sort(),
                      selected = current_selection())
  })

  observeEvent(input$measure, {
    current_selection(input$indicator)

    updateSelectInput(session, "indicator", choices = data %>%
                        filter(measure == input$measure,
                               type %in% c("Gross domestic product",
                                           "GDP per capita", 
                                           "GDP per hour worked",
                                           "Gross value added per hour worked market sector")) %>%
                        pull(type) %>%
                        unique() %>%
                        sort(),
                      selected = current_selection())
  })
  
  output$plot <- renderPlotly({
    
    lag_n <- ifelse(input$quarterly == "Quarter", 4, 1)
    tooltip_units <- ifelse(input$indicator == "GDP per capita", 1, 1e6)
    
    df <- data %>%
      group_by(type, series_type, measure) %>%
      mutate(period_change = (value/lag(value, n = lag_n, order_by = date)) - 1) %>%
      filter(type == input$indicator,
             series_type %in% input$series_type,
             measure == input$measure,
             date >= input$date[1],
             date <= input$date[2],
             !is.na(period_change)) 
    
    if(length(input$series_type)>1) {
      p <- ggplot(df, aes(x = date, y = period_change, col = series_type, group = 1, text = 
                            str_c("Measure: ", input$indicator, " (",series_type,")",
                                  "<br>Date: ", format(date, "%Y-%b"),
                                  "<br>", input$quarterly," Change: ", as_percent(100*period_change), 
                                  "<br>Value: ", as_comma(neg_round = 0, tooltip_units*value)))) + 
        geom_line() +
        scale_y_continuous(labels = percent_format()) + 
        labs(x = NULL, y = NULL) 
    } else {
      p <- ggplot(df, aes(x= date, y = period_change, text = 
                            str_c("Measure: ", input$indicator, "(", series_type, ")",
                                  "<br>Date: ", format(date, "%Y-%b"),
                                  "<br>", input$quarterly, " Change: ", as_percent(100*period_change), 
                                  "<br>Value: ", as_comma(neg_round = 0, tooltip_units*value)))) +  
        geom_bar(stat = 'identity', fill = aiti_colours['dark blue']) + 
        scale_y_continuous(labels = percent_format()) + 
        labs(x = NULL, y = NULL) 
        
    }
    
    ggplotly(p, tooltip = 'text')
  })
}