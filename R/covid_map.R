covidUI <- function(id, data) {
  ns <- NS(id)
  
  
  indicator_choices <- c("JobKeeper Applications (SA2)" = "jobkeeper_apps",
                         "JobKeeper Rate (% Businesses) (SA2)" = "jobkeeper_proportion",
                         "JobSeeker Payments (SA2)" = "jobseeker_payment",
                         "JobSeeker Rate (% Labour Force) (SA2)" = "jobseeker_proportion", 
                         "Payroll Jobs Index (SA4)" = "payroll_index")
  
  tabPanel(title = uiOutput(ns("title_panel")),
           shinycssloaders::withSpinner(leafletOutput(ns("map"), width = "100%", height = "600px"), 
                                        image = "https://github.com/hamgamb/aitidash/blob/master/www/aiti_spinner.gif?raw=true"),
           fluidRow(
             box(width = 4, status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "covid_impact"
                   ),
                 uiOutput(ns("date"))
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
           )
  )
  }

covidServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$title_panel = renderText({
        region()
      })
      
      output$date <- renderUI({
        
        if(input$indicator == "payroll_index") {
        sliderTextInput(
          inputId = session$ns("date"),
          label = "Weeks Since 100th COVID-19 Case (Mar 14th 2020)", 
          choices = data %>% 
            filter(indicator == "payroll_index") %>% 
            distinct(date) %>% 
            mutate(index = week(date) - week(as.Date("2020-03-14"))) %>%
            pull(index),
          selected = 0
        ) } else {
          
          sliderTextInput(
            inputId = session$ns("date"),
            label = "Select Date", 
            choices = data %>% filter(indicator != "payroll_index") %>%
              pull(date) %>% unique() %>% zoo::as.yearmon(date),
            selected = data %>% filter(indicator != "payroll_index") %>%
              pull(date) %>% unique() %>% zoo::as.yearmon(date) %>% max()
          )
        }
          
          })
      
      observe({
        if(region() != "Australian Capital Territory") {
          updateSelectInput(session, "indicator", choices = c("JobKeeper Applications (SA2)" = "jobkeeper_apps",
                                                                         "JobKeeper Rate (% Businesses) (SA2)" = "jobkeeper_proportion",
                                                                         "JobSeeker Payments (SA2)" = "jobseeker_payment",
                                                                         "JobSeeker Rate (% Labour Force) (SA2)" = "jobseeker_proportion", 
                                                                         "Payroll Jobs Index (SA4)" = "payroll_index"))
        } else {
        updateSelectInput(session, "indicator", choices = c("JobKeeper Applications (SA2)" = "jobkeeper_apps",
                                                            "JobKeeper Rate (% Businesses) (SA2)" = "jobkeeper_proportion",
                                                            "JobSeeker Payments (SA2)" = "jobseeker_payment",
                                                            "JobSeeker Rate (% Labour Force) (SA2)" = "jobseeker_proportion"))
        }
      })
      
      
      indicator_choices <- c("JobKeeper Applications (SA2)" = "jobkeeper_apps",
                             "JobKeeper Rate (% Businesses) (SA2)" = "jobkeeper_proportion",
                             "JobSeeker Payments (SA2)" = "jobseeker_payment",
                             "JobSeeker Rate (% Labour Force) (SA2)" = "jobseeker_proportion", 
                             "Payroll Jobs Index (SA4)" = "payroll_index"
                             )
    
      
      create_data <- reactive({
        
        validate(need(input$date, message = FALSE))
        
        if (input$indicator == "payroll_index") {
          data <- data %>%
            select(-sa2_main_2016) %>%
            filter(date == as.Date("2020-03-14") + weeks(input$date))
          
          label_name <-  'sa4_name_2016'
          join <- sa42016
          join_by <- "sa4_code_2016"

          } 
        else {
          data <- data %>%
            select(-sa4_code_2016) %>%
            filter(date == as.Date(zoo::as.yearmon(input$date)))
          label_name <- 'sa2_name_2016'
          join <- sa22016 
          join_by <- "sa2_main_2016"
          }
        
        if (region() != "Australia") {
        
          df <- data %>% 
            filter(state_name_2016 == region(),
                   indicator == input$indicator) %>% 
            mutate(value_label = ifelse(grepl("proportion", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(indicator == "payroll_index", as_comma(value, digits = 2), value_label)) %>% 
            left_join(join, by = c(join_by, "state_name_2016")) %>%
            rename(label = all_of(label_name)) %>%
            st_as_sf() 
          
        } else {
          
          df <- data %>% 
            filter(indicator == input$indicator) %>% 
            mutate(value_label = ifelse(grepl("proportion", indicator), as_percent(value), as_comma(value)),
                   value_label = ifelse(indicator == "payroll_index", as_comma(value, digits = 2), value_label)) %>% 
            left_join(join, by = c(join_by, "state_name_2016")) %>%
            rename(label = all_of(label_name)) %>%
            st_as_sf() 
        } 
        
        
      })
      
      create_plot <- reactive({
        
        pal_domain <- create_data() %>%
          filter(indicator == input$indicator) %>%
          pull(value)
        
        legend_title <- names(indicator_choices)[indicator_choices == input$indicator]
        
      
        pal <- colorBin("Blues", pal_domain, 6, pretty = TRUE, na.color = aiti_grey)
       
        
        if (input$indicator == "payroll_index") {
          annodate <- tags$div(HTML(format(as.Date("2020-03-14") + weeks(input$date), "%B %d %Y")))
        } else { annodate <- tags$div(HTML(format(zoo::as.yearmon(input$date), "%B %Y"))) }
          
              
        leaflet(create_data()) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(pal_domain),
            weight = 1, 
            opacity = 0.5, 
            color = "black",
            dashArray = "",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 2, 
              color = aiti_blue,
              dashArray = "",
              fillOpacity = 0.7, 
              bringToFront = TRUE),
            label = ~paste0(label, ": ", value_label)) %>%
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
          paste(region(), "-map.png", sep = '')
        },
        content = function(file) {
         mapshot(user_map(), file = file, cliprect = "viewport", selfcontained = FALSE)
        }
      )
      
   
      
      
      
    }
  )
}