#Module UI Function
#Labour Market tab/tabPanel

retailTradeUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    filter(series_type == "Trend",
           region == "South Australia") %>%
    pull(type) %>%
    unique() %>%
    sort()
  
  industry_choices <- sort(unique(data$industry))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "South Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(box(status ='info',solidHeader = TRUE,
                         selectInput(
                           inputId = ns('industry'), 
                           label = "Select Industry", 
                           choices = industry_choices,
                           selected = "Total (Industry)"))
           )
  )
                  
}

retailTrade <- function(input, output, session, data) {
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  columns <- colnames(data)
  
  output$plot <- renderPlotly({
    
    df  <- data %>% filter(
      region == "South Australia",
      industry == input$industry) %>%
      mutate(quarter = ceiling_date(date, 'quarter') + months(2)) %>%
      group_by(quarter) %>%
      summarise(value = mean(value)) %>%
      mutate(qrt_growth = (value-lag(value))/value) %>%
      filter(!is.na(qrt_growth))

  
    p = ggplot(df, aes(x = quarter, 
                       y = qrt_growth,
                       text = str_c('Date: ', format(quarter, "%b-%Y"),
                                    '<br>',input$industry,': ', formatC(100*qrt_growth, format = 'f', digits = 1), "%"), 
                                     
                       group = 1)) +
      geom_bar(stat = 'identity', fill=aiti_colours['dark blue'], size=0.5) +
      labs(
        x = NULL,
        y = "Quarterly Growth"
      ) +
      theme(legend.position = 'none') +
      scale_x_date(date_breaks = '5 years', date_labels = format("%Y")) +
      scale_y_continuous(labels = percent_format(accuracy = 1))
      
    ggplotly(p, tooltip='text')
    
    
    
  })
}