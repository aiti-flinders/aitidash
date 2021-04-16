covidRegionUI <- function(id, data) {
  ns <- NS(id)
  
  state_choices <- sort(unique(data$state))
  
  tabPanel(title = "Regional Comparison",
           plotlyOutput(ns("plot"), width = "100%", height = "600px"),
           fluidRow(
             dashboard_box(title = "Customise Chart",
                           selectInput(
                             width = "100%",
                             inputId = ns("indicator"),
                             label = "Select Indicator",
                             choices = c("Payroll Jobs Index" = "payroll_jobs",
                                         "Payroll Wages Index" = "payroll_wages"),
                             selected = "payroll_jobs"
                           ),
                           radioGroupButtons(
                             inputId = ns("facet"),
                             label = "Select Facet Variable",
                             choices = c("Gender" = "gender", 
                                         "Age" = "age",
                                         #"Industry" = "industry",
                                         "None" = "none"),
                             selected = "none",
                             direction = "horizontal",
                             justified = TRUE
                           )
                           
             ),
             dashboard_box(title = "Add Regions",
                           checkboxGroupButtons(
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

covidRegionServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        if(input$facet == "none") {
          df <- data %>%
            filter(industry == "All Industries",
                   indicator == input$indicator,
                   gender == "Persons",
                   age == "All ages",
                   state %in% input$state)
        } else if (input$facet == "gender") {
          df <- data %>% 
            filter(industry == "All Industries",
                   age == "All ages",
                   indicator == input$indicator, 
                   state %in% input$state)
        } else if (input$facet == "industry") {
          df <- data %>%
            filter(age == "All ages",
                   gender == "Persons", 
                   indicator == input$indicator, 
                   state %in% input$state) %>%
            mutate(industry = as_factor(industry)) 
          } else {
          df <- data %>%
            filter(industry == "All Industries",
                   gender == "Persons",
                   indicator == input$indicator,
                   state %in% input$state)

        }
      })
      
      create_plot <- reactive({
        
        plot_title <- case_when(
          input$indicator == "payroll_jobs" ~ "Payroll Jobs Index",
          input$indicator == "payroll_wages" ~ "Payroll Wages Index"
        )
        
        plot_title <- ifelse(length(input$state) <= 1,
                               toupper(paste0(plot_title, ": ", input$state)),
                               toupper(paste0(plot_title, ":  Multiple Regions")))
     
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
          geom_hline(aes(yintercept = 100)) + 
          geom_point(shape = 1, size = 1) + 
          theme_aiti(legend = "bottom", base_family = "Roboto") +
          theme(strip.background = element_blank()) + 
          aiti_colour_manual(n = length(input$state) + 1) 
      
         
        if(input$facet != "none") {
          p <- p + 
            facet_wrap(~get(input$facet)~., nrow = 5, ncol = 4) +
            scale_x_date(date_breaks = "4 months", date_labels = "%b") +
            labs(x = NULL,
                 title = plot_title)
        } else {
          p <- p + 
            scale_x_date(date_breaks = "4 months", date_labels = "%b") +
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