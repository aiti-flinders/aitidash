covidRegionUI <- function(id, data) {
  ns <- NS(id)
  
  state_choices <- sort(unique(data$state))
  
  tabPanel(title = "Regional Comparison",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             box(status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("facet"),
                   label = "Select Facet Variable",
                   choices = c("Gender" = "gender", 
                               "Age" = "age",
                               "None" = "none"),
                   selected = "none"
                 )),
             box(status = "info", solidHeader = FALSE,
                 checkboxGroupInput(
                   inputId = ns('state'),
                   label = "Select Comparison Region",
                   choices = state_choices)),
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
                   ))
             )
           )
  )
}

covidRegionServer <- function(id, data, region) {
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        if(input$facet == "none") {
          df <- data %>%
            filter(industry == "All Industries",
                   gender == "Persons",
                   age == "All ages",
                   state %in% c(region(), input$state))
        } else if (input$facet == "gender") {
          df <- data %>% 
            filter(industry == "All Industries",
                   age == "All ages",
                   state %in% c(region(), input$state))
        } else {
          df <- data %>%
            filter(industry == "All Industries",
                   gender == "Persons",
                   state %in% c(region(), input$state))
        }
      })
      
      observeEvent(region(), {
        
        updateCheckboxGroupInput(session, "state", choices = data %>%
                                   filter(state != region()) %>%
                                   pull(state) %>%
                                   unique() %>%
                                   sort())
      })
      
      create_plot <- reactive({
        
        plot_title <- ifelse(length(input$state) < 1,
                               toupper(paste0("Payroll Jobs Index: ", region())),
                               toupper(paste0("Payroll Jobs Index: Multiple Regions")))
     
        p <- ggplot(create_data(), 
                    aes(x = date, 
                        y = value, 
                        col = state, 
                        group = state,
                        text = paste0(
                          state, 
                          "<br>Week Ending: ", format(date, "%B %d"),
                          "<br>Index: ", as_comma(value, digits = 2)))) + 
          geom_line() +
          geom_point(shape = 1, size = 1) + 
          theme_aiti(legend = "bottom", base_family = "Roboto") +
          aiti_colour_manual(n = length(input$state) + 1) 
      
         
        if(input$facet != "none") {
          p <- p + 
            facet_wrap(~get(input$facet)~., nrow = 2, ncol = 4) +
            scale_x_date(date_breaks = "2 months", date_labels = "%b") +
            labs(x = NULL,
                 y = NULL)
        } else {
          p <- p + 
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            labs(x = NULL, 
                 y = NULL,
                 title = plot_title)
        }
        
        ggplotly(p, tooltip = "text") %>%
          layout(autosize = TRUE,
                 legend = list(orientation = "h", 
                               y = -0.15),
                 annotations = list(
                   x = 1,
                   y = -0.20,
                   text = "Source: AITI WorkSight",
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
          paste("payroll", "-plot.png", sep = '')
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste("payroll", "-data.csv", sep = '')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
      
    }
  )
}