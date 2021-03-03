#' @export
covidDemographicUI <- function(id, data) {
  ns <- NS(id)
  
  tabPanel("Demography",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             box(status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("breakdown"),
                   label = "Select Demographic Variable",
                   choices = c("Gender" = "gender",
                               "Age" = "age")
                 ))
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

#' @export
covidDemographicServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        
        if(input$breakdown == "age") {
          df <- data %>%
            filter(state == region(), 
                   industry == "All Industries",                 
                   indicator == "payroll_jobs",
                   gender == "Persons",
                   age != "All ages") 
        } else {
          df <- data %>%
            filter(state == region(),
                   industry == "All Industries",
                   indicator == "payroll_jobs",
                   age == "All ages", 
                   gender != "Persons")
        }
      })
      

      create_plot <- reactive({

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
                 paste0("Payroll Jobs Index: ", region())
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