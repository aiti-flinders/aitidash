covidIndustryUI <- function(id, data) {
  
  ns <- NS(id)
  
  industry_choices <- unique(data$industry)
  
  tabPanel("Industry",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             box(status = "info", solidHeader = FALSE,
                 uiOutput(ns("date"))),
             box(status = "info", solidHeader = FALSE,
                 checkboxGroupInput(
                   inputId = ns("industry"),
                   label = "Select Industry (Up to 9)",
                   choices = industry_choices,
                   selected = NULL
                 )
             )
           ),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = FALSE,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'
                 ),
                 downloadButton(
                   outputId = ns("download_data"),
                   label = "Click here to download the chart data",
                   class = 'download-button'
                 )
             )
           )
  )
}
             

covidIndustryServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        if(is.null(input$industry)) {
        df <- data %>%
          filter(state == region(),
                 gender == "Persons",
                 age == "All ages",
                 industry != "All industries",
                 date == as.Date("2020-03-14") + weeks(input$date)) 
        } else {
          df <- data %>%
            filter(state == region(),
                   gender == "Persons",
                   age == "All ages",
                   industry %in% input$industry)
        }
      })
      
      output$date <- renderUI({
        if(is.null(input$industry)) {
          sliderTextInput(
            inputId = session$ns("date"),
            label = "Weeks Since 100th COVID-19 Case (Mar 14th 2020)", 
            choices = data %>% 
              distinct(date) %>% 
              mutate(index = week(date) - week(as.Date("2020-03-14"))) %>%
              pull(index),
            selected = data %>% 
              distinct(date) %>% 
              mutate(index = week(date) - week(as.Date("2020-03-14"))) %>%
              pull(index) %>% max())
        }
      })
      
      create_plot <- reactive({
        
        if(is.null(input$industry)) {
          plot_title <- case_when(
            input$date == 1 ~ paste0("Change in Payroll Jobs Index: ", region(), " (", input$date, " week since March 14th", ")"),
            input$date > 1 ~ paste0("Change in Payroll Jobs Index: ", region(), " (", input$date, " weeks since March 14th", ")"),
            input$date == -1 ~ paste0("Change in Payroll Jobs Index: ", region(), " (", -as.numeric(input$date), " week before March 14th", ")"),
            input$date < -1 ~ paste0("Change in Payroll Jobs Index: ", region(), " (", -as.numeric(input$date), " weeks before March 14th", ")"),
            TRUE ~ paste0("Change in Payroll Jobs Index: ", region(), " (Week Ending March 14th)")
            )
          

          p <- ggplot(create_data(),
              aes(x = reorder(industry, -(value-100)),
                  y = value-100,
                  text = paste0(region(), 
                                "<br>Week Ending: ", format(date, "%d %B %Y"),
                                "<br>Index: ", as_comma(value, digits = 2)))) +
          geom_bar(stat = 'identity', fill = aiti_blue) + 
          scale_y_continuous(labels = percent_format(scale = 1)) + 
          coord_flip() +
          theme_aiti(legend = "bottom", base_family = "Roboto") + 
          labs(y = NULL, title = toupper(plot_title))
          
        } else {
          
          plot_title <- ifelse(length(input$industry) > 1,
                               toupper(paste0(region(), ": ", "Payroll Jobs Index", " (Multiple industries)")),
                               toupper(paste0(region(), ": ", "Payroll Jobs Index", " (", input$industry, ")")))
          

    
          p <- ggplot(create_data(),
                      aes(x = date,
                          y = value,
                          col = industry,
                          group = industry,
                          text = paste0(region(), 
                                        "<br>Week Ending: ", format(date, "%d %B %Y"),
                                        "<br>Index: ", as_comma(value, digits = 2)))) +
            geom_line()  + 
            geom_point(shape = 1, size = 1) + 
            theme_aiti(legend = "bottom", base_family = "Roboto") + 
            scale_x_date(date_breaks = "2 weeks", date_labels = "%b-%d") + 
            aiti_colour_manual(n = length(input$industry)) +
            labs(x = NULL, y = NULL, title = plot_title)
          
        }
        
        ggplotly(p, tooltip = "text") %>%
          layout(autosize = TRUE,
                 legend = list(orientation = "h", 
                               y = -0.15),
                 annotations = list(
                   x = 1,
                   y = -0.20,
                   text = "Source: AITI Economic Indicators",
                   showarrow = FALSE,
                   xref = "paper",
                   yref = "paper",
                   xanchor = "right", 
                   yanchor ="right"
                 ))

      })
      
      output$plot <- renderPlotly({
        create_plot()
        
      }) 
      
      output$download_plot <- downloadHandler(
        filename = function(){
          paste("Payroll Index", region(),"plot.png", sep = '-')
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("Payroll Index", region(), "data.csv", sep = '-')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
    }
      
  )
}