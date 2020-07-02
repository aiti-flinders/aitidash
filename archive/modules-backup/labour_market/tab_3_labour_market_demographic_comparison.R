##Demographic Module


labourMarketDemogUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    filter(state == "South Australia") %>%
    pull(indicator) %>%
    unique() %>%
    sort()
  
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
                   selected = "Trend")),
             box(status = 'info', solidHeader = TRUE,
                 numericInput(
                   inputId = ns("date_range"),
                   label = 'Select Base Year',
                   value = 2010,
                   min = year(date_min),
                   max = year(date_max),
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
  
  current_selection <- reactiveVal(NULL)
  
  observeEvent(region(), {
    
    updateSelectInput(session, "demographic", choices = if(region() == "Australia") c("Age", "Gender") else("Gender"))
    
  })
  
  observeEvent(input$indicator, {
    
    current_selection(input$series_type)
    
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(state == region(),
                               indicator == input$indicator) %>%
                        pull(series_type) %>%
                        unique() %>%
                        sort(), 
                      selected = current_selection())
  })
  
  observeEvent(input$series_type, {
    
    current_selection(input$indicator) 
    
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region(),
                               series_type == input$series_type) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(),
                      selected = current_selection())
  })
  
  output$plot <- renderPlotly({
    df <-  data %>% 
      filter(indicator == input$indicator,
             state == region(),
             series_type == input$series_type,
             year >= input$date_range, 
             age == "Total (age)") %>% 
      group_by(gender, date) %>% 
      summarise(value = mean(value),
                unit = first(unit)) %>%
      mutate(index = 100*value/value[1]) %>%
      ungroup() 
    
    if(df$unit[1] == "000") {
      p <- ggplot(df, aes(x = date, y = index, col = gender,
                          text = str_c('Gender: ', gender,
                                       "<br>Date: ", format(date, "%Y-%b"),
                                       "<br>", input$indicator, ": ", as_comma(index, neg_round = 0), ' (Index) ',
                                       as_comma_group(df, group = 'gender'), ' (Value)'),
                          group = 1)) + 
        geom_line(size = 0.25) + 
        labs(
          x = NULL,
          y = str_c("Index (Base: ", month(min(df$date), abbr = F, label = T), year(min(df$date)), "=100)")
        ) + 
        scale_colour_aiti() 
        
    } else { p <- ggplot(df, aes(x = date, y = value, col = gender, 
                       text = str_c('Gender: ', gender,
                                    '<br>Date: ',format(date, "%Y-%b"),
                                    '<br>', input$indicator, ": ", as_percent(value)),
                       group = 1)) + 
      geom_line(size = 0.25) + 
      labs(
        x=NULL,
        y=input$indicator
      ) +
      theme(legend.position = 'none') +
      scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1)) +
      scale_colour_aiti() }
    
    ggplotly(p, tooltip='text')
  })
}