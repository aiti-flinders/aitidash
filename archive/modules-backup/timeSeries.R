#Module UI Function
#Labour Market tab/tabPanel

labourMarketUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    filter(series_type == "Trend",
           region == "South Australia") %>%
    pull(type) %>%
    unique() %>%
    sort()
  
  series_choices <- sort(unique(data$series_type))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "South Australia", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status ='info',solidHeader = TRUE,
                 selectInput(
                   inputId = ns('indicator'),
                   label = "Select Indicator", 
                   choices = indicator_choices),
                 selectInput(
                   inputId = ns('type'), 
                   label = "Select Series Type", 
                   choices = series_choices),
                 sliderInput(
                   inputId = ns('date'),
                   label = "Select Date Range",
                   min = date_min,
                   max = date_max,
                   value = c(date_min, date_max),
                   timeFormat = "%Y-%b"
                 )),
             box(status='info',solidHeader = TRUE,
                 tableOutput(ns('table'))),
             box(status='info', solidHeader = TRUE,
                 uiOutput(ns('analysis')))
           ))
}

labourMarket <- function(input, output, session, data) {
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  columns <- colnames(data)
  
  output$plot <- renderPlotly({
    
    df  <- data %>% filter(
      type == input$indicator,
      region == "South Australia",
      series_type == input$type,
      date >= input$date[1],
      date <= input$date[2])%>%
      {if("gender" %in% names(.)) filter(., gender == "Persons") else .}
    
  
    p = ggplot(df, aes(x = date, 
                       y = value,
                       text = paste0('Date: ', format(date, "%Y-%b"),
                                     '<br>',input$indicator,': ', 
                                     if(df$unit[1]=="Percent"){str_c(formatC(value, format = 'f', digits = 1), "%")}
                                     else{str_c(formatC(value, format = 'f', digits = 0, big.mark=","))}),
                       group = 1)) +
      geom_line(colour="#fde06d", size=0.5) +
      labs(
        x = NULL,
        y=input$indicator
      ) +
      scale_color_manual(name = NULL,
                         values = c("#56B4E9"),
                         breaks = c("Persons"),
                         labels = c("Persons"),
                         drop = FALSE) +
      theme(legend.position = 'none',
            axis.text.x=element_text(angle=90, vjust=0.5, size=8),
            panel.grid.minor = element_blank()) +
      
      if(df$unit[1]=="Percent"){label = scale_y_continuous(label=function(x) paste0(x,"%"))}
    else{scale_y_continuous(label = comma_format(scale = 1/as.numeric(df$unit[1])))}
    ggplotly(p, tooltip='text')
    
    
    
  })
  output$table <- renderTable(align = 'c', expr = {
    data %>%
      filter(month == "February",
             type == input$indicator,
             region == "South Australia",
             series_type == "Trend",
             year == year(date_max) | year == year(date_max)-1) %>%
      {if("gender" %in% names(.)) filter(., gender == "Persons") else .} %>%      
      select(year, value) %>%
      spread(key = year, value = value) %>%
      mutate(`% Change` = `2019`-`2018`)
    
  })
  output$analysis <- renderUI({
    analysis(data = data, 
             indic = input$indicator, 
             s_type = input$type) 
  })
}