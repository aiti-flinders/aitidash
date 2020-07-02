#Regional Comparisons


retailTradeRegionalUI <- function(id, data) {
  ns <- NS(id)
  
  industry_choices <- data %>% 
    pull(industry) %>%
    unique() %>%
    sort()
  
  region_choices <-  data %>%
    pull(region) %>%
    unique() %>%
    sort()
  
  min_date <- year(min(data$date))
  max_date <- year(max(data$date))
  
  region_choices <- region_choices[region_choices!= "South Australia"]

  
  tabPanel(title = "Region", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status= 'info',solidHeader=TRUE,
                 selectInput(
                   inputId = ns('industry'), 
                   label = "Select Industry",
                   choices = industry_choices,
                   selected = "Total (Industry)"),
                 radioButtons(
                   inputId = ns('quarterly'),
                   label = NULL,
                   choices = c("Quarterly Growth", "Annual Growth"),
                   selected = "Annual Growth"),
                 sliderInput(
                   inputId = ns('date'),
                   label = NULL,
                   value = c(min_date, max_date),
                   sep = "",
                   min = min_date,
                   max = max_date)),
             box(status='info',solidHeader=TRUE,
                 checkboxGroupInput(
                   inputId = ns('states'),
                   label = "Select States",
                   choices = region_choices)))
           
          
  )  
} 

retailTradeRegional <- function(input, output, session, data) {
  
  # date_min <- min(data$date)
  # date_max <- max(data$date)
  
  output$plot <- renderPlotly({
    
  
    df <- data %>% 
      filter(industry == input$industry,
             region %in% c("South Australia", input$states)) %>%
      mutate(quarter = ceiling_date(date, 'quarter')) %>%
      group_by(quarter, region) %>%
      mutate(value_qrt = mean(value)) %>%
      ungroup() %>%
      group_by(year, region) %>%
      mutate(value_year = mean(value)) %>%
      ungroup() %>%
      mutate(qrt_growth = (value_qrt-lag(value_qrt))/value_qrt,
             year_growth = (value_year-lag(value_year))/value_year) %>%
      filter(!is.na(qrt_growth) | !is.na(year_growth),
             year(date) >= input$date[1],
             year(date) <= input$date[2])
    if(input$quarterly == "Quarterly Growth") {
      df <- df %>% 
        group_by(quarter, region) %>%
        summarise(value = mean(qrt_growth)) %>%
        rename(date = quarter)
      
      tooltip_format <- "%Y-%b"
      
    } else { 
      df <- df %>%
        mutate(year = ymd(year, truncated = 2L)) %>%
        group_by(year, region) %>%
        summarise(value = mean(year_growth)) %>%
        rename(date = year)
        
        tooltip_format <- "%Y"
        
      }
    
    

    p <- ggplot(df, aes(x = date,
                        y = value,
                        fill = region,
                       text = str_c('Date: ', format(date, tooltip_format),
                                    '<br>', region, 
                                    '<br>',input$industry,': ', formatC(100*value, format = 'f', digits = 1), "%"))) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(
        x = NULL,
        y = NULL
        ) +
      scale_fill_aiti() +
      scale_y_continuous(labels = percent_format(scale = 100, accuracy = 0.1)) +
      theme(legend.position = 'none')

    
    ggplotly(p, tooltip = 'text')
    
  })
}