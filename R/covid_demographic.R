covidDemographicUI <- function(id, data) {
  ns <- NS(id)
  
  tabPanel("Demographic Comparison",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             dashboard_box(title = "Customise Chart",
                           selectInput(
                             inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = c("Payroll Jobs Index" = "payroll_jobs",
                                         "Payroll Wages Index" = "payroll_wages"),
                             selected = "payroll_jobs"
                           ),
                           radioGroupButtons(
                             inputId = ns("breakdown"),
                             label = "Select Demographic Variable",
                             choices = c("Gender" = "gender",
                                         "Age" = "age")
                           )
             ),
             dashboard_box(width = 8, title = "Downloads",
                           download_graph_ui(id)
             )
           )
  )
  
}

covidDemographicServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        
        if(input$breakdown == "age") {
          df <- data %>%
            filter(state == region(), 
                   industry == "All Industries", 
                   indicator == input$indicator, 
                   gender == "Persons",
                   age != "All ages") 
        } else {
          df <- data %>%
            filter(state == region(),
                   indicator == input$indicator,
                   industry == "All Industries",
                   age == "All ages", 
                   gender != "Persons")
        }
      })
      

      create_plot <- reactive({
        
        plot_title <- case_when(
          input$indicator == "payroll_jobs" ~ "Payroll Jobs Index",
          input$indicator == "payroll_wages" ~ "Payroll Wages Index"
        )

        p <-  ggplot(create_data(), 
                     aes_(x = ~date, 
                          y = ~value, 
                          colour = as.name(input$breakdown),
                          text = ~paste0(region(), 
                                         "<br>Week Ending: ", format(date, "%d %B %Y"),
                                         "<br>Index: ", as_comma(value, digits = 2)),
                          group = as.name(input$breakdown))) + 
          geom_line() +
          geom_point(shape = 1, size = 1) + 
          theme_aiti(legend = 'bottom', base_family = "Roboto") +
          scale_x_date(date_breaks = "4 weeks", date_labels = "%b-%d") + 
          aiti_colour_manual(8) +
          labs(x = NULL,
               y = NULL,
               title = toupper(
                 paste0(plot_title, ": ", region())
               ))
        
        
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
          paste("Payroll Index", region(), input$breakdown,"plot.png", sep = '-')
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("Payroll Index", region(), input$breakdown,"data.csv", sep = '-')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
      
      
      

    }
  )
}