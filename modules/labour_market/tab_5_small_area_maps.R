
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
            ),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = FALSE,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'
                 )
           )
           ),
           fluidRow(
             verbatimTextOutput(outputId = ns("text"))
           )
  )
}

labourMarketSmallArea <- function(input, output, session, data, region) {  

  
  create_data <- reactive({
    
    if (region() == "Australia") {
    df <- data %>%
      filter(indicator == "Smoothed unemployment rate (%)",
             date == input$date) 
    } else {
      df <- data %>%
        filter(indicator == "Smoothed unemployment rate (%)",
               date == input$date,
               state_name_2016 == region())
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
          bringToFront = TRUE),
        label = ~str_c(sa2_name_2016,": ", value)) %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = create_data()$value, 
        title = "Unemployment Rate (%)")
    
  })


  
  output$map <- renderLeaflet({
    create_plot()
  })
  
  user_map <- reactive({
    create_plot() %>%
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom)
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste(region(), "-map.png", sep = '')
    },
    content = function(file) {
      mapview::mapshot(user_map(), file = file, cliprect = "viewport", selfcontained = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$indicator, "-data.csv", sep = '')
    },
    content = function(file) {
      write.csv(create_data(), file, row.names = FALSE)
    }
  )
  



}
