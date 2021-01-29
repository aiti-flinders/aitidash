
labourMarketSmallAreaUI <- function(id, data) {
  ns <- NS(id)
  
  date_select <- as.yearqtr(unique(data$date))
  indicator_select <- unique(data$indicator)

  
  tabPanel(title = "Small Area Labour Force",
           withSpinner(leafletOutput(ns("map"), width = "100%", height = "600px"),
                       image = "https://raw.githubusercontent.com/hamgamb/aitidash/master/inst/www/aiti_spinner.gif"),
           fluidRow(
             box(status = 'info', solidHeader = FALSE,
               sliderTextInput(
                   inputId = ns("date"),
                   label = "Select Date",
                   choices = date_select,
                   selected = max(date_select)
                 )
             ),
             box(status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_select
                 ))
            ),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = FALSE,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'
                 )
           )
           )
  )
}

labourMarketSmallAreaServer <- function(id, data, region) {
  moduleServer(
    id,
    function(input, output, session) {

  
  create_data <- reactive({
    
    if (region() == "Australia") {
    df <- data %>%
      filter(indicator == input$indicator,
             date == as.Date(as.yearqtr(input$date)) + months(2)) %>%
      mutate(value_label = ifelse(grepl("%", indicator), as_percent(value), as_comma(value))) %>%
      left_join(absmapsdata::sa22016) %>%
      st_as_sf()
    } else {
      df <- data %>%
        filter(indicator == input$indicator,
               date == as.Date(as.yearqtr(input$date)) + months(2),
               state_name_2016 == region()) %>%
        mutate(value_label = ifelse(grepl("%", indicator), as_percent(value), as_comma(value))) %>%
        left_join(absmapsdata::sa22016) %>%
        st_as_sf()
    }
  })
  
  create_plot <- reactive({
    
    pal_domain <- create_data()[create_data()$indicator == input$indicator, ]$value
    
    legend_title <- case_when(
      input$indicator == "Smoothed unemployment rate (%)" ~ "Unemployment Rate (%)",
      input$indicator == "Smoothed labour force (persons)" ~ "Labour Force",
      input$indicator == "Smoothed unemployment (persons)" ~ "Unemployment"
    )
    
    annodate <- tags$div(
      HTML(
        format(as.Date(as.yearqtr(input$date)) + months(2), "%B %Y")
        )
    )
    
    pal <- colorBin("Blues", pal_domain, 6, pretty = TRUE, na.color = aititheme::aiti_grey)
    
    leaflet(create_data()) %>%
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(pal_domain),
        weight = 1,
        opacity = 0.5,
        color = 'white',
        dashArray = "",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = aititheme::aiti_blue,
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0(sa2_name_2016,": ", value_label)) %>%
      addLegend(
        "bottomright", 
        pal = pal, 
        values = pal_domain, 
        title = legend_title) %>%
      addControl(annodate, position = "topright")
    
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
      paste(region(), "map.png", sep = '-')
    },
    content = function(file) {
      mapview::mapshot(user_map(), file = file, cliprect = "viewport", selfcontained = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$indicator, "data.csv", sep = '-')
    },
    content = function(file) {
      write.csv(create_data(), file, row.names = FALSE)
    }
  )
  
    }
)
}
