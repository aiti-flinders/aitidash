covidDemographicUI <- function(id, data) {
  ns <- NS(id)
  
  state_choices <- sort(unique(data$state))
  
  
  tabPanel("Demographic Comparison",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             dashboard_box(title = "Customise Chart",
                           selectInput(
                             inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = c("Payroll Jobs Index" = "Payroll jobs"),
                             selected = "Payroll jobs"
                           ),
                           radioGroupButtons(
                             inputId = ns("breakdown"),
                             label = "Select Demographic Variable",
                             choices = c("Sex" = "gender",
                                         "Age" = "age")
                           )
             ),
             dashboard_box(title = "Select Region",
                           radioGroupButtons(
                             inputId = ns('state'),
                             label = NULL,
                             choices = state_choices,
                             direction = "vertical",
                             justified = TRUE,
                             selected = "Australia"
                           )
             ),
             dashboard_box(title = "Downloads",
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
            filter(state == input$state, 
                   industry == "Total (industry)", 
                   indicator == input$indicator,
                   gender == "Persons",
                   age != "Total (age)") 
        } else {
          df <- data %>%
            filter(state == input$state,
                   indicator == input$indicator,
                   industry == "Total (industry)",
                   age == "Total (age)", 
                   gender != "Persons")
        }
      })
      

      create_plot <- reactive({
        
        plot_title <-  "Payroll Jobs Index"


        p <-  ggplot(create_data(), 
                     aes_(x = ~date, 
                          y = ~value, 
                          colour = as.name(input$breakdown),
                          text = ~paste0(input$state, 
                                         "<br>Week Ending: ", format(date, "%d %B %Y"),
                                         "<br>Index: ", as_comma(value, digits = 2)),
                          group = as.name(input$breakdown))) + 
          geom_line() +
          geom_hline(aes(yintercept = 100)) + 
          geom_point(shape = 1, size = 1) + 
          theme_aiti(legend = 'bottom', base_family = "Roboto") +
          scale_x_date(breaks = pretty_breaks(6), date_labels = "%B") + 
          aiti_colour_manual(8) +
          labs(x = NULL,
               y = NULL,
               title = toupper(
                 paste0(plot_title, ": ", input$state)
               ))
        
        
        ggplotly(p, tooltip = "text") %>%
          layout(autosize = TRUE,
                 legend = list(orientation = "h", 
                               y = -0.15,
                               title = ""),
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
          paste("Payroll Index", input$state, input$breakdown,"plot.png", sep = '-')
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("Payroll Index", input$state, input$breakdown,"data.csv", sep = '-')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
      
      
      

    }
  )
}