iviUI <- function(id, data){
  ns <- NS(id)
  
  occupation_choices <- data %>%
    mutate(occupation = ifelse(str_detect(occupation, "total"), "Total", occupation)) %>%
    pull(occupation) %>%
    unique() %>%
    sort()
  
  tabPanel(title = uiOutput(ns("title_panel")),
           plotlyOutput(ns("plot"), width = "100%"),
           fluidRow(
             box(status = "info", solidHeader = TRUE,
                 selectInput(
                   inputId = ns("occupation"),
                   label = "Select Occupation",
                   choices = occupation_choices
                 )),
             box(),
             box()
           )
  )
}

ivi <- function(input, output, session, data, region) {
  
  output$title_panel <- renderText({
    region()
  })
  
  
  create_data <- reactive({
    df <- data %>%
      filter(region == region(),
             anzsco_2 == 0)
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
      aiti_colour_manual(n = length(region())) +
      theme_aiti()
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", 
                           y = -0.15))
    
  })
  
  output$plot <- renderPlotly({
    create_plot()
  })
}