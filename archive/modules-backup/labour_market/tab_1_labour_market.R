#Module UI Function
#Labour Market tab/tabPanel


labourMarketUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>%
    pull(indicator) %>%
    unique() %>%
    sort()
  
  series_choices <- data %>%
    pull(series_type) %>%
    unique() %>%
    sort()
  
  
  
  tabPanel(title = uiOutput(ns("title_panel")),
           plotlyOutput(ns("plot"), width = '100%'),
           fluidRow(
             box(status = 'info', solidHeader = TRUE,
               selectInput(
                 inputId = ns("indicator"),
                 label = "Select Indicator",
                 choices = indicator_choices,
                 selected = "Unemployment rate"
               )),
             box(status = 'info', solidHeader = TRUE,
               selectInput(
                 inputId = ns("series_type"),
                 label = "Select Series Type",
                 choices = series_choices,
                 selected = "Seasonally Adjusted"
               )
             )
           ))
  
}



labourMarket <- function(input, output, session, data, region) {
  

  output$title_panel = renderText({
    region()
  })
  
  current_series_type <- reactiveVal(NULL)
  current_indicator <- reactiveVal(NULL)

  
  observeEvent(input$indicator, {
    
    current_series_type(input$series_type)
    
    updateSelectInput(session, "series_type", choices = data %>%
                        filter(state == region(),
                               indicator == input$indicator) %>%
                        pull(series_type) %>%
                        unique() %>%
                        sort(), 
                      selected = current_series_type())
    
  })
  
  observeEvent(input$series_type, {
    
    current_indicator(input$indicator) 
    
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region(),
                               series_type == input$series_type) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(),
                      selected = current_indicator())
  })
  
  observeEvent(region(), {
    
    updateSelectInput(session, 
                      "series_type", 
                      choices = data %>% 
                        filter(state == region(), indicator == "Unemployment rate") %>% 
                        pull(series_type) %>% 
                        unique() %>% 
                        sort(), 
                      selected = "Seasonally Adjusted")
    updateSelectInput(session, 
                      "indicator", 
                      choices = data %>% 
                        filter(state == region(), series_type == "Seasonally Adjusted") %>% 
                        pull(indicator) %>% 
                        unique() %>% 
                        sort(), 
                      selected = "Unemployment rate")
    
  })
  
  

  output$plot <- renderPlotly({
    
    df  <- data %>% 
      filter(indicator == input$indicator,
             state == region(),
             series_type == input$series_type,
             gender == "Persons", 
             age == "Total (age)")

  
    p = ggplot(df, aes(x = date, 
                       y = value,
                       text = str_c('Date: ', format(date, "%Y-%b"),
                                     '<br>',input$indicator,': ', 
                                     if(df$unit[1]=="Percent"){as_percent(value)}
                                     else{as_comma(value)}),
                       group = 1)) +
      geom_line(colour = aiti_colours['dark blue'], size=0.25) +
      labs(
        x = NULL,
        y = input$indicator,
        caption = 
      ) +
      scale_colour_aiti() + 
      theme(legend.position = 'none') +
      

      if(df$unit[1]=="Percent") {
        scale_y_continuous(label = percent_format(scale = 1, accuracy = 0.1))
      } else {
        scale_y_continuous(label = comma_format(scale = 1/1000, accuracy = 1, suffix = "k")) }
    ggplotly(p, tooltip='text') 
    
    
  })
  
  


}

