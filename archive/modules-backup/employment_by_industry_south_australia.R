#Employment by industry

empIndUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
   pull(indicator) %>%
    unique() %>%
    sort()
  
  industry_choices <- data %>%
    pull(industry) %>%
    unique() %>%
    sort()

  series_choices <- sort(unique(data$series_type))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = uiOutput(ns('title_panel')), plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Employed Total"),
                 sliderInput(
                   inputId = ns('date_range'),
                   label = "Select Year",
                   value = year(date_max),  
                   min = year(date_min),
                   max = year(date_max),
                   animate = T,
                   sep = "",
                   timeFormat = "%Y")),
             box(status = 'info', solidHeader = TRUE,
                 checkboxGroupInput(
                   inputId = ns('industry'),
                   label = "Select Industry",
                   choices = industry_choices
                 ))
           )
  )
}

empInd <- function(input, output, session, data, region) {
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  output$title_panel <- renderText({
    region()
  })
  
  current_indicator <- reactiveVal(NULL)
  
  observeEvent(input$employment_industry_tab_id, {
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region()) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(),
                      selected = "Employed full-time")
  })
  
  observeEvent(region(), {
    
    current_indicator(input$indicator)
    
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region()) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(), 
                      selected = current_indicator())
    
  })
  

  output$plot <- renderPlotly({
    
    if(is.null(input$industry)) {
      df <- data %>%
      filter(industry != "Total (industry)",
             state == region(), 
             indicator == input$indicator) %>% 
      group_by(year, industry) %>% 
      summarise(value = mean(value)) %>% 
      mutate(share = 100*value/sum(value),
             max_share = ifelse(share == max(share), "fill", 'NA'),
             max_value = ifelse(value == max(value), "fill", 'NA')) %>%
      filter(year == input$date_range) %>%
      arrange(desc(industry)) %>%
      mutate(industry = as_factor(industry))
    
    
    p <- ggplot(df, aes(x = reorder(industry, value), 
                        y = value, 
                        fill = max_value,
                        text = str_c(input$indicator, ": ", as_comma(value),
                                     " (", as_percent(share), ")"))) + 
      geom_bar(stat='identity') + 
      labs(
        y = NULL,
        x = NULL
      ) +
      scale_y_continuous(labels = comma_format(scale = 1/1000, suffix = 'k')) + 
      scale_fill_aiti()+
      coord_flip() +
      theme(legend.position = 'none')
    } else {
      df <- data %>% 
        filter(state == region(), 
               indicator == input$indicator) %>%
        group_by(date) %>%
        mutate(share = 100*value/sum(value)) %>%
        filter(industry %in% input$industry)
      
      p <- ggplot(df, aes(x = date, 
                          y = value, 
                          colour = industry, 
                          text = str_c("Date: ", format(date, "%Y-%b"),
                                       "<br>",industry, ": ", as_comma(value),
                                       " (", as_percent(share), ")"),
                          group = industry)) + 
        geom_line() + 
        labs(
          x = NULL, 
          y = "Employed"
        ) + 
        scale_colour_aiti() + 
        scale_y_continuous(labels = comma_format(scale = 1/1000, suffix = 'k')) +
        theme(legend.position = 'none')
        
    }

    ggplotly(p, tooltip = 'text')
  })
  
}