iviComparisonUI <- function(id, data) {
  ns <- NS(id)
  
  state_choices <- sort(unique(data$region))
  
  
  tabPanel("Regional Comparison",
           width = "100%", 
           plotlyOutput(ns("plot")),
           fluidRow(
             box(status = "info", 
                 solidHeader = FALSE, 
                 uiOutput(ns("occupation_group_select")),
                 uiOutput(ns("occupation_select"))),
             box(status = "info",
                 solidHeader = FALSE,
                 checkboxGroupInput(
                   inputId = ns('state'),
                   label = "Select Comparison Region",
                   choices = state_choices))),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = FALSE,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'),
                 downloadButton(
                   outputId = ns("download_data"),
                   label = "Click here to download the chart data",
                   class = 'download-button')
             )
           )
  )
}
                   
                  

iviComparisonServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      occupation_choices <- reactive({
        validate(
          need(input$occupation_group, message = FALSE)
        )
        data %>%
          filter(occupation_group == input$occupation_group, 
                 region == region()) %>%
          pull(occupation) %>%
          unique() %>%
          sort()
      })
      
      occupation_group_choices <- data %>%
          pull(occupation_group) %>%
          unique() %>%
          sort()
      
      
      output$occupation_group_select <- renderUI({
        selectInput(inputId = session$ns("occupation_group"),
                    label = "Select Occupation Group",
                    choices = occupation_group_choices,
                    selected = "Total")
      })
      
      output$occupation_select <- renderUI({
        selectInput(inputId = session$ns("occupation"),
                    label = "Select Occupation",
                    choices = occupation_choices(),
                    selected = "TOTAL")
      })
      
      observeEvent(region(), {
        updateCheckboxGroupInput(session, "state", choices = data %>% 
                                   filter(region !=region()) %>% 
                                   pull(region) %>% 
                                   unique() %>%
                                   sort(), label = "Select Comparison region")
      })
      
      create_data <- reactive({
        validate(
          need(input$occupation, message = FALSE),
          need(input$occupation_group, message = FALSE)
        )
        df <- data %>%
          filter(occupation == input$occupation,
                 occupation_group == input$occupation_group,
                 region %in% c(region(), input$state)) %>%
          group_by(occupation, occupation_group, region) %>%
          mutate(index = 100*vacancies/vacancies[1]) %>%
          ungroup()
      })
      
      create_plot <- reactive({
    
    
        plot_title <- toupper(paste0("Internet Vacancies: ", 
                                         paste0(region(), collapse = " & "), 
                                         " (", input$occupation_group, ")",
                                         " (", input$occupation, ")"))
        p <- ggplot(create_data(),
                    aes(x = date, y = index, group = 1, colour = region,
                        text = paste0(
                          region(), 
                          "<br>Date: ", format(date, "%Y-%b"),
                          "<br>Occupation Group: ", input$occupation_group,
                          "<br>Occupation: ", input$occupation,
                          "<br>Vacancies: ", as_comma(vacancies)))) + 
          geom_line() + 
          labs(
            x = NULL,
            y = NULL, 
            title = plot_title
            ) +
          scale_y_continuous(labels = comma_format()) +       
          aiti_colour_manual(n = length(input$state) + 1) +
          theme_aiti(legend = "bottom", base_family = "Roboto")
        
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
                   yanchor ="auto"
                 ))      
        
        })
      
      output$plot <- renderPlotly({
        
        validate(
          need(nrow(create_data() > 0), message = FALSE),
          need(input$occupation_group, message = FALSE)
        )
        create_plot()
      })
      
      
      output$download_plot <- downloadHandler(
        filename = function(){
          paste(input$occupation, "-plot.png", sep = '')
        },
        content = function(file) {
          plotly_IMAGE(create_plot(), out_file = file)
        }
      )
      
      output$download_data <- downloadHandler(
        filename = function() {
          paste(input$occupation, "-data.csv", sep = '')
        },
        content = function(file) {
          write.csv(create_data(), file, row.names = FALSE)
        }
      )
      
      
      
    }
  )
}