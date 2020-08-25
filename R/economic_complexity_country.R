complexityStatesUI <- function(id, data) {
  ns <- NS(id) 
  
  indicator_choices <- data %>%
    pull(indicator) %>%
    unique() %>%
    sort() %>%
    setNames(nm = c("Complexity Outlook", "Diversity", "Complexity Index", "Total Exports"))
  
  tabPanel("Country Indicators",
           plotlyOutput(ns("plot"), height = "600px"),
           fluidRow(
             box(width = 6, status = "info", solidHeader = FALSE,
                 selectInput(inputId = ns("indicator"),
                             label = "Select Indicator", 
                            choices = indicator_choices,
                            selected = "Complexity Index")
                 ))
           )
}

complexityStatesServer <- function(id, region, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        df <- data %>%
          filter(indicator == input$indicator)
      })
      
      output$plot <- renderPlotly({
        p <- ggplot(create_data(), 
                    aes(x = year, 
                        y = value, 
                        col = location_code,
                        text = )) + 
          geom_line() +
          theme_aiti(legend = "bottom", base_family = "Roboto") +
          aiti_colour_manual(n = 8) +
          labs(x = NULL)
        
        ggplotly(p) %>%
          layout(autosize = TRUE,
                           legend = list(orientation = "h",
                                         y = -0.15),
                           annotations = list(
                             x = 1,
                             y = -0.2,
                             showarrow = FALSE,
                             xref = "paper",
                             yref = "paper",
                             xanchor = "right",
                             yanchor = "auto",
                             text = "Source: AITI Economic Indicators"
                           ))
        
      })
      
    }
  )
  
}