
labourMarketSmallAreaUI <- function(id, data) {
  ns <- NS(id)

  
  tabPanel(title = "Small Area Unemployment Rates",
           leafletOutput(ns("map"), width = "100%", height = "600px"),
           fluidRow(
             box(status = 'info', solidHeader = FALSE,
               selectInput(
                   inputId = ns("date"),
                   label = "Select Date",
                   choices = unique(data$date),
                   selected = max(data$date)
                 )
             )
            )
  )
}

labourMarketSmallArea <- function(input, output, session, data, region) {  
  
  create_data <- reactive({
    df <- data %>%
      filter(indicator == "Smoothed unemployment rate (%)",
             date == input$date) 
    
    if (region() != "Australia") {
      df <- df %>%
        filter(state_name_2016 == region())
    }  
    
    })
  
  create_plot <- reactive({
    
    pal <- colorBin("viridis", create_data()$value, 8, pretty = TRUE)
    
    leaflet(create_data()) %>%
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(value),
        weight = 1,
        opacity = 0.5,
        color = 'white',
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = aiti_blue,
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)) %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = create_data()$value, 
        title = "Unemployment Rate (%)")
    
  })


  
  output$map <- renderLeaflet({
    create_plot()
  })
  



}
