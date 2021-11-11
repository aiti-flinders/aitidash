covidIndustryUI <- function(id, data) {
  
  ns <- NS(id)
  indicator_choices <- c("Payroll Jobs Index" = "payroll_jobs")
  industry_choices <- unique(data$industry)
  
  tabPanel(title = "Industry",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             dashboard_box(title = "Customise Chart",
                           selectInput(
                             inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = indicator_choices,
                             selected = "payroll_jobs"
                           ),
                           pickerInput(
                             inputId = ns("industry"),
                             label = "Select Industry (max 9)",
                             choices = industry_choices,
                             multiple = TRUE
                           ),
                           uiOutput(ns("date"))
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
             

covidIndustryServer <- function(id, data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        if(is.null(input$industry)) {
        df <- data %>%
          filter(state == input$state,
                 sex == "Persons",
                 age == "All ages",
                 industry != "All industries",
                 indicator == input$indicator,
                 date == as.Date("2020-03-14") + weeks(input$date)) 
        } else {
          df <- data %>%
            filter(state == input$state,
                   sex == "Persons",
                   indicator == input$indicator,
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
              mutate(index = as.integer(difftime(date, "2020-03-14", units = "weeks"))) %>%
              pull(index),
            selected = data %>% 
              distinct(date) %>% 
              mutate(index = as.integer(difftime(date, "2020-03-14", units = "weeks"))) %>%
              pull(index) %>% max())
        }
      })
      
      create_plot <- reactive({
        
        plot_title <- "Payroll Jobs Index"

        
        if(is.null(input$industry)) {
          plot_title <- case_when(
            input$date == 1 ~ paste0("Change in ", plot_title, ": ", input$state, " (", input$date, " week since March 14th", ")"),
            input$date > 1 ~ paste0("Change in ", plot_title, ": ", input$state, " (", input$date, " weeks since March 14th", ")"),
            input$date == -1 ~ paste0("Change in ", plot_title, ": ", input$state, " (", -as.numeric(input$date), " week before March 14th", ")"),
            input$date < -1 ~ paste0("Change in ", plot_title, ": ", input$state, " (", -as.numeric(input$date), " weeks before March 14th", ")"),
            TRUE ~ paste0("Change in ", plot_title, " :", input$state, " (Week Ending March 14th)")
            )
          

          p <- ggplot(create_data(),
              aes(x = reorder(industry, -(value-100)),
                  y = value-100,
                  text = paste0(input$state, 
                                "<br>Week Ending: ", format(date, "%d %B %Y"),
                                paste0("<br>", plot_title, ": "), as_comma(value, digits = 2)))) +
          geom_bar(stat = 'identity', fill = aititheme::aiti_blue) + 
          scale_y_continuous(labels = percent_format(scale = 1)) + 
          coord_flip() +
          theme_aiti(legend = "bottom", base_family = "Roboto") + 
          labs(y = NULL, title = toupper(plot_title))
          
        } else {
          
          plot_title <-  "Payroll Jobs Index"
          
          plot_title <- ifelse(length(input$industry) > 1,
                               toupper(paste0(plot_title, ": ", input$state, " (Multiple industries)")),
                               toupper(paste0(plot_title, ": ", input$state, " (", input$industry, ")")))
          

    
          p <- ggplot(create_data(),
                      aes(x = date,
                          y = value,
                          col = industry,
                          group = industry,
                          text = paste0(input$state, 
                                        "<br>Week Ending: ", format(date, "%d %B %Y"),
                                        "<br>Index: ", as_comma(value, digits = 2)))) +
            geom_line()  + 
            geom_hline(aes(yintercept = 100)) + 
            geom_point(shape = 1, size = 1) + 
            theme_aiti(legend = "bottom", base_family = "Roboto") + 
            scale_x_date(date_breaks = "4 weeks", date_labels = "%b-%d") + 
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
          paste0(input$filename, input$state, "-plot.", input$filetype)
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), format = input$filetype, width = input$width, height = input$height, out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste(input$indicator, input$state, "-data.csv", sep = '')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
    }
      
  )
}