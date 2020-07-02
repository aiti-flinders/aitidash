#Population Pyramids

populationDemoUI <- function(id, data) {
  ns <- NS(id)
  
  date_max <- max(data$date)
  date_min <- min(data$date)
  
  tabPanel(title = "Demographic", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 sliderInput(
                   inputId = ns('date'),
                   label = "Select Year",
                   value = year(date_max), 
                   min = year(date_min),
                   max = year(date_max),
                   animate = T,
                   sep = "",
                   timeFormat = "%Y"
                 )),
             box(status = 'info', solidHeader = TRUE,
                 radioButtons(
                   inputId = ns("state"),
                   label = "Select Region",
                   choices = unique(data$region)))
           ))
}

populationDemo <- function(input, output, session, data) {
  
  output$plot <- renderPlotly({
    df <- data %>%
      group_by(year, gender, region, age_group) %>%
      summarise(value = mean(value)) %>%
      filter(gender != "Persons",
             year == input$date,
             region == input$state) %>%
      mutate(share = ifelse(gender == "Male", 100*value/sum(value), -100*value/sum(value)))  
     
    
    p <- ggplot(df, aes(x=age_group, y = share, fill = gender,
                        text = str_c(gender," aged: ", age_group,
                                     "<br> Proportion: ", str_c(formatC(share, format = 'f', digits = 1), "%")))) + 
      geom_bar(stat = 'identity') + 
      scale_fill_aiti() + 
      # geom_bar(data = df %>%
      #            filter(gender == "Male"), 
      #          aes(x=age_group, y = share, 
      #              text = str_c("Males aged: ", age_group,
      #                           "<br> Proportion: ", str_c(formatC(share, format = 'f', digits = 1), "%"))), 
      #          stat = 'identity', fill = aiti_colours['dark blue']) +
      # geom_bar(data = df %>% 
      #            filter(gender == "Female"), 
      #          aes(x=age_group, y =-share,
      #              text = str_c("Females aged: ", age_group, 
      #                           "<br> Proportion: ", str_c(formatC(share, format = 'f', digits = 1), "%"))),
      #          stat = 'identity', fill = aiti_colours['orange']) +
      labs(y = "Proportion of Total Population",
           x = "Age Group") + 
      coord_flip() + 
      scale_y_continuous(breaks = seq(-10,10,2.5),
                         labels = str_c(as.character(c(seq(10,0,-2.5), seq(2.5,10, 2.5))), "%")) +
      theme(legend.position = 'none') 
      

    ggplotly(p, tooltip = 'text')
    
  })
}
