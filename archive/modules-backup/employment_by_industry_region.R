#Employment by industry

empIndComparisonUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    pull(indicator) %>%
    unique() %>%
    sort()
  
  series_choices <- sort(unique(data$series_type))
  region_choices <- sort(unique(data$state))
  region_choices <- region_choices[region_choices!= "Australia"]
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Regional Comparison", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator", 
                   choices = indicator_choices),
                 sliderInput(
                   inputId = ns('date_range'),
                   label = "Select Year",
                   value = year(date_max), 
                   min = year(date_min),
                   max = year(date_max),
                   animate = T,
                   sep = "",
                   timeFormat = "%Y"
                 )),
             box(status='info',solidHeader = TRUE,
                 radioButtons(
                   inputId = ns("comparison"),
                   label = "Select Region to Compare with Australia",
                   choices = region_choices)
             )))
}

empIndComparison <- function(input, output, session, data) {
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  output$plot <- renderPlotly({
    #Total Employment
    df <- data %>%
      filter(indicator == input$indicator) %>% 
      group_by(year, state, industry) %>% 
      summarise(value = mean(value)) %>% 
      mutate(share = 100*value/sum(value)) %>%
      filter(state %in% c("Australia", input$comparison),
             year == input$date_range) %>%
      select(-value) %>% 
      spread(key = state, value = share) %>%
      mutate(order = (`Australia` - !!as.name(input$comparison)),
             difference = !!as.name(input$comparison) - `Australia`) %>%
      arrange(desc(industry)) %>%
      mutate(industry = factor(industry, industry))
      
            
    
      p <- ggplot(df) + 
        geom_segment(aes(x=industry, xend = industry, y = difference, yend=0), colour = '#a1a1a1') + 
        geom_point(aes(x=industry, y = difference, 
                       text = str_c(industry,
                                     "<br>",input$comparison,": ",as_percent(!!as.name(input$comparison)), " of ", input$indicator)),
                   shape = 21, colour = aiti_colours['orange']) +
        geom_point(aes(x=industry, y = 0,
                       text = str_c(industry,
                                    "<br>", "Australia: ", as_percent(`Australia`)," of ", input$comparison)),
                   shape = 21, colour = aiti_colours['blue']) + 
        scale_y_continuous(breaks = c(-8, -4, 0, 4, 8), labels = percent_format(scale = 1, accuracy = 0.1), limits = c(-10, 10)) +
        coord_flip() + 
        labs(
          y = NULL,
          x = NULL 
        ) 
        
    
    ggplotly(p, tooltip = 'text')
  })
}