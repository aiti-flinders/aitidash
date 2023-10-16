payroll_jobs_ui <- function(id, data) {
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
                             choices = c("Payroll Jobs Index" = "Payroll jobs",
                                         "Payroll Wages Index" = "Payroll wages"),
                             selected = "Payroll jobs"
                           ),
                           radioGroupButtons(
                             inputId = ns("facet"),
                             label = "Select Facet Variable",
                             choices = c("Gender" = "gender", 
                                         "Age" = "age",
                                         "Industry" = "industry",
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

payroll_jobs_server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        
        if(input$facet == "none") {
          data %>%
            filter(industry == "Total (industry)",
                   indicator == input$indicator,
                   gender == "Persons",
                   age == "Total (age)",
                   state %in% input$state)
          
        } else if (input$facet == "gender") {
          data %>% 
            filter(industry == "Total (industry)",
                   age == "Total (age)",
                   indicator == input$indicator, 
                   state %in% input$state)
          
        } else if (input$facet == "industry") {
          data %>%
            filter(age == "Total (age)",
                   gender == "Persons", 
                   industry != "Total (industry)",
                   indicator == input$indicator, 
                   state %in% input$state) %>%
            mutate(industry = as_factor(industry)) 
          
        } else if (input$facet == "age") {
          
          data %>%
            filter(industry == "Total (industry)",
                   gender == "Persons",
                   age != "Total (age)",
                   indicator == input$indicator,
                   state %in% input$state)
          
        }
        
      })
      
      create_plot <- reactive({
        
        if (input$facet == "none") {
          
          abs_plot(data = create_data(),
                   type = "line",
                   indicator = input$indicator,
                   state = input$state,
                   compare_aus = FALSE,
                   plotly = TRUE)
          
        } else if (input$facet == "gender") {
          
          abs_plot(data = create_data(),
                   type = "line",
                   indicator = input$indicator,
                   state = input$state,
                   sex = c("Males", "Females"),
                   facet = "gender",
                   compare_aus = FALSE,
                   plotly = TRUE)
          
        } else if (input$facet == "industry") {
          
          abs_plot(data = create_data(),
                   type = "line",
                   indicator = input$indicator,
                   state = input$state,
                   industry = unique(create_data()$industry),
                   facet = "industry",
                   compare_aus = FALSE,
                   plotly = TRUE)
          
        } else if (input$facet == "age") {
          
          abs_plot(data = create_data(),
                   type = "line",
                   indicator = input$indicator,
                   states = input$state,
                   ages = unique(create_data()$age),
                   industries = "Total (industry)",
                   facet = "age",
                   compare_aus = FALSE,
                   plotly = TRUE)
        }
        
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