#National/State Accounts
industryUI <- function(id, data) {
  ns <- NS(id)
  tabPanel(title = "South Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 selectInput(inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = c("Industry Contribution",
                                         "Gross State Product")
                             )),
             box(status = 'info', solidHeader = TRUE,
                 numericInput(inputId = ns('year'),
                              label = "Select Year",
                              min = min(data$year), 
                              max = max(data$year),
                              value = 2018))))
                              
}

industry <- function(input, output, session, id, data) {
  
  output$plot <- renderPlotly({

  
  if(input$indicator == "Gross State Product") {
    df <- data %>%
      filter(type == input$indicator) %>%
      mutate(annual_change = (value/lag(value, n = 1, order_by = date))-1)
    
    p <- ggplot(df, aes(x = year, y = annual_change, text = str_c("Year: ", year, 
                                                                  "<br>GSP: ", as_comma(value), 
                                                                  "<br>GSP Growth: ", as_percent(100*annual_change)))) + 
      geom_bar(fill = aiti_colours['dark blue'], stat = 'identity') +
      scale_y_continuous(labels = percent_format(scale = 100, accuracy = 0.1)) + 
      labs(x = NULL, y = "Annual Change in Gross State Product") 

  } else {
    df <- data %>% 
      filter(type == input$indicator,
             sector != "Total all industries") %>% 
      group_by(year) %>%
      mutate(share = value/sum(value)) %>%
      group_by(sector) %>%
      mutate(share_shift = (share/lag(share)) -1) %>%
      filter(year == input$year)
    
    p <- ggplot(df, aes(x = reorder(sector, -share), y = share, text = str_c("Contribution to GSP: ", as_comma(value),
                                                                                   "<br>Share of GSP: ", as_percent(100*share)))) + 
      geom_bar(stat = 'identity', fill = aiti_colours['dark blue']) + 
      scale_y_continuous(labels = percent_format(scale = 100, accuracy = 0.1)) +
      labs(x = NULL, y = "Contribution to Gross State Product") +
      coord_flip()
    
      
  }
  
  ggplotly(p, tooltip = 'text')
  
  })
}
  
  
# state_accs <- read_abs("5220.0", 1) %>%
#   separate(series, into = c('region', 'indicator'), sep = ';', extra = 'drop') %>%
#   mutate_if(is.character, trimws)
# 
# state_accs %>%
#   filter(indicator == "Gross state product: Chain volume measures") %>%
#   group_by(region) %>%
#   mutate(annual_change = (value / lag(value, n = 4, order_by = date)) -1) %>%
#   ggplot(aes(x = date, y = annual_change, col = region)) + 
#   geom_line() + 
#   scale_colour_aiti()
# 
# sa_gsp <- read_abs('5220.0', tables = 5) %>%
#   separate(series, into = c('sector', 'indicator'), sep = ';', extra = 'drop') %>%
#   mutate_if(is.character, trimws)