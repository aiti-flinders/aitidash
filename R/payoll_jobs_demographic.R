payroll_jobs_demographic_ui <- function(id, data) {
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

payroll_jobs_demographic_server <- function(id, data, region) {
  
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
        
        if (input$breakdown == "age") {
          
          abs_plot(create_data(),
                   type = "line",
                   indicator = input$indicator,
                   industry = "Total (industry)",
                   state = input$state,
                   age = unique(create_data()$age),
                   sex = "Persons",
                   plotly = TRUE)
        } else {
          
          abs_plot(create_data(),
                   type = "line",
                   indicator = input$indicator,
                   industry = "Total (industry)",
                   state = input$state,
                   age = "Total (age)",
                   sex = c("Males", "Females"),
                   plotly = TRUE)
          
        }
        
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