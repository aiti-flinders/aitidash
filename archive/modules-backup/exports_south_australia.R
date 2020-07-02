exportsUI <- function(id, data) {
  ns <- NS(id)
  
  country_choices <- data %>%
    pull(destination) %>%
    unique()
  
  tabPanel(title = "South Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status ='info',solidHeader = TRUE,
                 selectInput(
                   inputId = ns('destination'),
                   label = "Select Destination Country", 
                   choices = country_choices,
                   selected = "Total (Country of Destination)"))))
}

exports <- function(input, output, session, data) {
  
  output$plot <- renderPlotly({
    
    df <- data %>% 
      filter(destination == input$destination) %>%
      mutate(ma_12 = zoo::rollapply(value, 12, sum, align = 'right', fill = 'drop')) %>%
      filter(!is.na(ma_12))
    
    p <- ggplot(df, 
                aes(x=date, y = ma_12, text = str_c("Date: ", format(date, "%Y"), "<br> Value: ", ma_12),group = 1)
                ) + 
      geom_line(size = 0.25) + 
      labs(x = NULL, y = "12 Month Total Exports") + 
      scale_fill_aiti()
    
    ggplotly(p, tooltip = 'text')
    
  })
}