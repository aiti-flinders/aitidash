empIndDetailedUI <- function(id, data) {
  
  ns <- NS(id)
  
  indicator_choices <- data %>%
    pull(indicator) %>%
    unique() %>%
    set_names(c("Employed full-time",
                "Employed part-time",
                "Hours worked (employed full-time)",
                "Hours worked (employed part-time)"))
  
  tabPanel("Detailed",
           width = "100%",
           plotlyOutput(ns("plot"), height = "600px"),
           fluidRow(
             box(status = "info",
                 solidHeader = FALSE,
                 selectInput(
                   inputId = ns("division"),
                   label = "Select Industry Division",
                   choices = data %>% pull(division) %>% unique() %>% sort()
                 )),
             box(status = "info",
                 solidHeader = FALSE,
                 uiOutput(ns("subdivision_select")))),
           fluidRow(
             box(status = "info",
                 solidHeader = FALSE, 
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_choices
                 )),
             box(status = "info",
                 solidHeader = FALSE,
                 selectInput(
                   inputId = ns("gender"),
                   label = "Select Gender",
                   choices = data %>% pull(gender) %>% unique() %>% sort()
                 ))
           )
           
  )
  
  
}

empIndDetailedServer <- function(id, data, region) {
  moduleServer(
    id,
    function(input, output, session) {
      
      subdivisions <- reactive({
        validate(
          need(input$indicator, message = FALSE),
          need(input$division, message = FALSE)
        )
        data %>%
          filter(indicator == input$indicator,
                 division == input$division) %>%
          pull(subdivision) %>%
          unique() %>%
          sort()
      })
      

      output$subdivision_select <- renderUI({
        
        selectInput(inputId = session$ns("subdivision"),
                    label = "Select Industry Subdivision",
                    choices = subdivisions())
        
      })
      
      
      
      create_data <- reactive({
        validate(
          need(input$subdivision, message = FALSE)
        )
        data %>%
          filter(indicator == input$indicator,
                 region == region(),
                 division == input$division,
                 subdivision == input$subdivision)
      })
      
      create_plot <- reactive({
        validate(
          need(nrow(create_data()) > 0, message = FALSE)
        )
        p <- ggplot(create_data(), 
                    aes(x = date, 
                        y = value,
                        group = 1, 
                        col = gender,
                        text = str_c(input$indicator, "<br>",
                                     "Subdivision: ", input$subdivision, "<br>",
                                      as_comma(1000*value), " (", input$gender, ")"))) + 
          geom_line() + 
          theme_fof() + 
          scale_y_continuous(labels = comma_format(scale = 1000)) + 
          labs(
            x = NULL,
            title = str_c(input$indicator, input$gender, input$division, input$subdivision, sep = " ")
          )
        
        ggplotly(p, tooltip = 'text')
      })
      
      output$plot <- renderPlotly({
        create_plot()
      })
      
    }
  )
}