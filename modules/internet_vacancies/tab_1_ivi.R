iviUI <- function(id, data){
  ns <- NS(id)
  
  occupation_groups <- data %>%
    pull(occupation_group) %>%
    unique() %>%
    sort()
  
  occupation_choices <- data %>%
    pull(occupation) %>%
    unique() %>%
    sort()
  
  tabPanel(title = uiOutput(ns("title_panel")),
           plotlyOutput(ns("plot"), width = "100%"),
           fluidRow(
             box(status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("occupation_group"),
                   label = "Select Occupation Group",
                   choices = occupation_groups
                 )),
             box(status = "info", solidHeader = FALSE,
                 selectInput(
                   inputId = ns("occupation"),
                   label = "Select Occupation",
                   choices = occupation_choices
                 )),
             box()
           )
  )
}

ivi <- function(input, output, session, data, region) {
  
  output$title_panel <- renderText({
    region()
  })
  
  observeEvent(input$occupation_group,  {
  updateSelectInput(session, "occupation", choices = data %>%
      filter(occupation_group == input$occupation_group) %>%
      pull(occupation) %>%
      unique() %>%
      sort()
  )})
  
  create_data <- reactive({
    df <- data %>%
      filter(region == region(),
             occupation == input$occupation)
  })
  
  create_plot <- reactive({
    
    plot_title <- str_to_upper(str_c(region(),": ", "Internet Vacancies"))
    p <- ggplot(create_data(), aes(x = date, 
                                   y = vacancies,
                                   text = str_c("Date: ", format(date, "%Y-%b"),
                                                "<br>", input$occupation, ": ", as_comma(vacancies)),
                                   group = occupation)) +
      geom_line() +
      labs(
        x = NULL,
        y = NULL,
        title = plot_title
      ) + 
      scale_y_continuous(labels = comma_format()) + 
      aiti_colour_manual(n = length(input$occupation)) +
      theme_aiti(base_family = "Roboto")
    
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
}