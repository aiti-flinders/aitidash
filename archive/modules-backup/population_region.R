#Population and other demographics
populationRegionUI <- function(id, data) {
  ns <- NS(id)
  
region_choices <- data %>%
  pull(region) %>% 
  unique() %>%
  sort()

region_choices <- region_choices[region_choices != "South Australia"]
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Region", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Population Indicator",
                   choices = c("Population", "Population Growth"),
                   selected = "Population"),
                 radioButtons(
                   inputId = ns('quarterly'),
                   label = NULL,
                   choices = c("Quarterly", "Yearly"), 
                   selected = "Quarterly")),
                 
             box(status = 'info', solidHeader = TRUE,
                 radioButtons(
                   inputId = ns("state"),
                   label = "Select Region to Compare with South Australia",
                   choices = region_choices)
             )
           )
  )
} 

populationRegion <- function(input, output, session, data) {
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  output$plot <- renderPlotly({

    
    df <- data %>%
      filter(gender == "Persons",
             region %in% c("South Australia",input$state)) %>%
      # group_by(date, region) %>%
      # #mutate(value_qrt = mean(value)) %>%
      # ungroup() %>%
      # group_by(year, region) %>%
      # mutate(value_year = mean(value)) %>%
      # ungroup() %>%
      group_by(region) %>%
      mutate(growth_qrt = (value / lag(value, n = 1, order_by = date)) -1,
             growth_year = (value /lag(value, n = 4, order_by = date)) -1) %>%
      gather(key = "indicator", value = "value", c(value, growth_qrt, growth_year)) %>%
      mutate(type = ifelse(str_detect(pattern = "value", string = indicator), "Population", "Population Growth")) %>%
      filter(type == input$indicator)
    
    if(input$quarterly == "Quarterly") {
      df <- df %>%
        group_by(region, date) %>%
        summarise(value = mean(value)) %>%
        mutate(index = 100*value/value[1])
      
      tooltip_format_date <- "%Y-%b"
      
    } else {
      df <- df %>%
        mutate(year = ymd(year, truncated = 2L)) %>%
        group_by(region, year) %>%
        summarise(value = mean(value)) %>%
        mutate(index = 100*value/value[1]) %>%
        rename(date = year)
      
      tooltip_format_date <- "%Y"
    }
    
    
    if(input$indicator == "Population") {
      p <- ggplot(df, aes(x = date, y = index, colour = region, group = 1,
                        text = str_c(region, "<br>Date: ", format(date, tooltip_format_date), 
                                     "<br>", input$indicator, ": ", as_comma_group(df, group = 'region')))) +
        geom_line() +
        labs(y = str_c("Index (Base: ", format(date, tooltip_format), " = 100)"), 
             x = NULL) +
        scale_colour_aiti() + 
        theme(legend.position = "none")
    
    
    
    } else {
      p <- ggplot(df, aes(x = date, y = value, fill = region, 
                          text = str_c(region, "<br>Date: ", format(date, tooltip_format_date), 
                                       "<br>", input$indicator, ": ", as_percent(100*value)))) +
        geom_bar(stat = 'identity', position = 'dodge') +
        scale_fill_aiti() + 
        scale_y_continuous(labels = percent_format(scale = 100, accuracy = 0.1)) + 
        labs(y = str_c(input$quarterly, "Growth"), x = NULL) + 
        theme(legend.position = "none")
      
    }
    
    ggplotly(p, tooltip = 'text') 
    
    
  })
}



