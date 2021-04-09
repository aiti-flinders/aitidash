map_ui <- function(id, data, title) {
  ns <- NS(id)
  
  indicator_choices <- data %>%
    distinct(indicator) %>%
    pull()
  
  tabPanel(title = title,
           fluidRow(
             leafletOutput(ns("map"), width = "100%", height = "600px"),
             dashboard_box(title = "Customise Chart",
                           selectInput(
                             inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = indicator_choices,
                           ),
                           uiOutput(
                             ns("date")
                           )
             ),
             dashboard_box(title = "Select Region",
                           radioGroupButtons(
                             inputId = ns("state"),
                             label = NULL,
                             choices = regions(),
                             selected = "Australia",
                             direction = "vertical",
                             justified = TRUE
                           )
             ),
             dashboard_box(title = "Downloads",
                           download_graph_ui(id)
             )
           )
  )
}

map_server <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$date <- renderUI({
        
        if (input$indicator == "Payroll Index") {
          sliderTextInput(
            width = "100%",
            inputId = session$ns("date"),
            label = "Select Number of Weeks Since 14th March 2020 (Australia's 100th COVID-19 Case)",
            choices = data %>%
              distinct(date) %>%
              mutate(index = as.integer(difftime(date, "2020-03-14", units = "weeks"))) %>%
              pull(index),
            selected = data %>%
              distinct(date) %>%
              mutate(index = as.integer(difftime(date, "2020-03-14", units = "weeks"))) %>%
              pull(index) %>%
              max()
          )
        } else if (grepl("Smoothed", input$indicator)) {
          sliderTextInput(
            width = "100%",
            inputId = session$ns("date"),
            label = "Select Date",
            choices = data %>%
              pull(date) %>%
              unique() %>%
              as.yearqtr(),
            selected = data %>%
              pull(date) %>%
              unique() %>%
              as.yearqtr() %>% 
              max()
            )
        } else {
          sliderTextInput(
            width = "100%",
            inputId = session$ns("date"),
            label = "Select Date",
            choices = data %>%
              filter(indicator == input$indicator, !is.na(value)) %>%
              distinct(date) %>% pull() %>% as.yearmon(),
            selected = data %>% 
              filter(indicator == input$indicator, !is.na(value)) %>%
              distinct(date) %>% pull() %>% as.yearmon() %>% max()
          )
        }
        
      })
      
      create_data <- reactive({
        validate(need(input$date, message = FALSE))
        
        if (input$indicator == "Payroll Index") {
          data <- data %>%
            filter(!is.na(sa3_code_2016),
                   date == as.Date("2020-03-14") + weeks(input$date)) %>%
            select(-state_name_2016)
          
          label_name <- "sa3_name_2016"
          join <- absmapsdata::sa32016
          join_by <- "sa3_code_2016"
          
        } else if (grepl("Smoothed", input$indicator)) {
          data <- data %>%
            filter(!is.na(sa2_main_2016),
                   date == as.Date(as.yearqtr(input$date)) + months(2)) %>%
            select(-sa2_name_2016, 
                   -state_name_2016)
          
          label_name <- "sa2_name_2016"
          join <- absmapsdata::sa22016
          join_by <- "sa2_main_2016"
        }
        
        else {
          data <- data %>%
            filter(!is.na(sa2_main_2016),
                   date == as.Date(as.yearmon(input$date)))
          
          label_name <- "sa2_name_2016"
          join <- absmapsdata::sa22016
          join_by <- "sa2_main_2016"
        }
        
        if (input$state != "Australia") {
          data <- data %>%
            filter(indicator == input$indicator) %>%
            mutate(value_label = ifelse(grepl("proportion", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(grepl("%", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(indicator == "payroll_index", as_comma(value, digits = 2), value_label)) %>%
            left_join(join, by = join_by) %>%
            filter(state_name_2016 == input$state) %>%
            rename(label = all_of(label_name)) %>%
            st_as_sf() %>%
            st_transform("+proj=longlat +datum=WGS84") 
            
        } else {
          data <- data %>%
            filter(indicator == input$indicator) %>%
            mutate(value_label = ifelse(grepl("proportion", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(grepl("%", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(indicator == "payroll_index", as_comma(value, digits = 2), value_label)) %>%
            left_join(join, by = join_by) %>%
            rename(label = all_of(label_name)) %>%
            st_as_sf() %>%
            st_transform("+proj=longlat +datum=WGS84") 
            
            
        }
      })
      
      create_plot <- reactive({
        pal_domain <- create_data() %>%
          pull(value)
        
        legend_title <- input$indicator
        
        pal <- colorBin("Blues", pal_domain, 6, pretty = TRUE, na.color = aititheme::aiti_grey)
        
        if (input$indicator == "Payroll Index") {
          annodate <- tags$div(HTML(format(as.Date("2020-03-14") + weeks(input$date), "%B %d %Y")))
        } else if (grepl("Smoothed", input$indicator)) {
          annodate <- tags$div(HTML(format(as.Date(as.yearqtr(input$date)) + months(2), "%B %Y")))
        } else { 
          annodate <- tags$div(HTML(format(as.yearmon(input$date), "%B %Y"))) 
        }
        
        leaflet(create_data()) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(pal_domain),
            weight = 1,
            opacity = 0.6,
            color = "black",
            dashArray = "",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 2,
              color = aititheme::aiti_blue,
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = ~paste0(label, ": ", value_label)) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = pal_domain,
            title = legend_title) %>%
          addControl(
            annodate,
            position = "topright")
       
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
          paste0(input$filename, "-plot.", input$filetype)
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), format = input$filetype, width = input$width, height = input$height, out_file = file)
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
  )
  
}