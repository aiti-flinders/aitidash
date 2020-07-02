#Population and other demographics
populationUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    pull(type) %>%
    unique() %>%
    sort()
  
  series_choices <- sort(unique(data$series_type))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "South Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info', solidHeader = TRUE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Population Indicator",
                   choices = c("Population", "Population Growth"),
                   selected = "Population"),
                 radioButtons(
                   inputId = ns('quarterly'),
                   label = NULL,
                   choices = c("Quarterly", "Yearly"), 
                   selected = "Quarterly")
             )
           )
  )
}

population <- function(input, output, session, data) {
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  #There is potentially an issue with the quarterly pop-growth data - spikes in March
  output$plot <- renderPlotly({
    df <- data %>%
      filter(gender == "Persons",
             region == "South Australia") %>%
      group_by(date) %>%
      mutate(value_qrt = mean(value)) %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(value_year = mean(value)) %>% 
      ungroup() %>% 
      mutate(growth_qrt = (value_qrt-lag(value_qrt))/value_qrt,
             growth_year = (value_year - lag(value_year))/value_year) %>%
      gather(key = "indicator", value = "value", c(value_qrt, value_year, growth_qrt, growth_year)) %>%
      mutate(type = ifelse(str_detect(pattern = "value_", string = indicator), "Population", "Population Growth")) %>%
      filter(type == input$indicator) 
    
    if(input$quarterly == "Quarterly") {
      df <- df %>%
        filter(str_detect(string = indicator, pattern = "_qrt")) %>%
        group_by(date,region) %>%
        summarise(value = mean(value)) 
      
      tooltip_format_date <- "%Y-%b"
      
    } else {
      df <- df %>%
        mutate(year = ymd(year, truncated = 2L)) %>%
        filter(str_detect(string = indicator, pattern = "_year")) %>%
        group_by(year, region) %>%
        summarise(value = mean(value)) %>%
        rename(date = year)
      
      tooltip_format_date <- "%Y"
    }
    
      p <- ggplot(df, aes(x = date, y = value, fill = region, group = 1,
                        text = str_c("Date: ", format(date, tooltip_format_date), 
                                     "<br>", input$indicator, ": ", if(input$indicator =="Population Growth"){
                                       str_c(formatC(100*value, format = 'f', digits = 1), "%")} else {
                                         formatC(value, format = 'f', digits = 0, big.mark = ',')}))) + 
      geom_line() +
      scale_fill_aiti() + 
      labs(y = input$indicator, x = NULL) + 
      scale_y_continuous(labels = if(input$indicator == "Population Growth"){
        percent_format(scale = 100, accuracy = 0.1)} else {number_format(scale = 1/1000, suffix = 'k', big.mark = ',')}) + 
      theme(legend.position = "none")


    ggplotly(p, tooltip = 'text')
      
      
  })
}



