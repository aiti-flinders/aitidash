  #Regional Comparisons


labourMarketRegionalUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- data %>% 
    pull(indicator) %>%
    unique() %>%
    sort()
  
  series_choices <- sort(unique(data$series_type))
  state_choices <- sort(unique(data$state))
  state_choices <- state_choices[state_choices!="Australia"]
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Regional Comparison", plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status= 'info',solidHeader=TRUE,
                 selectInput(
                   inputId = ns('indicator'), 
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Unemployment rate"),
                 selectInput(
                   inputId = ns('series_type'),
                   label =  "Select Series Type", 
                   choices = series_choices,
                   selected = "Seasonally Adjusted"),
                 numericInput(
                   inputId = ns('date_range'),
                   label = 'Select Base Year',
                   value = 2010,
                   min = year(date_min),
                   max = year(date_max),
                   step = 1)),
             box(status='info',solidHeader=TRUE,
                 checkboxGroupInput(
                   inputId = ns('state'),
                   label = "Select Comparison Region",
                   choices = state_choices))
             )
           )
} 

labourMarketRegional <- function(input, output, session, data, region) {
  
  current_selection <- reactiveVal(NULL)

  observeEvent(input$indicator, {

    current_selection(input$series_type)

    updateSelectInput(session, "series_type", choices = data %>%
                        filter(state == "Australia",
                               indicator == input$indicator) %>%
                        pull(series_type) %>%
                        unique() %>%
                        sort(),
                      selected = current_selection())
    updateCheckboxGroupInput(session, "state", choices = data %>% 
                               filter(indicator == input$indicator, series_type == input$series_type, state != region()) %>% 
                               pull(state) %>% unique() %>% sort(), selected = NULL)
  })

  observeEvent(input$series_type, {

    current_selection(input$indicator)

    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == "Australia" ,
                               series_type == input$series_type) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(),
                      selected = current_selection())
    updateCheckboxGroupInput(session, "state", choices = data %>% 
                               filter(indicator == input$indicator, series_type == input$series_type, state != region()) %>% 
                               pull(state) %>% unique() %>% sort(), selected = NULL)
  })
  
  observeEvent(region(), {
    
    updateSelectInput(session, "series_type", choices = data %>% filter(state == region(), indicator == "Unemployment rate") %>% pull(series_type) %>% unique() %>% sort(), selected = "Seasonally Adjusted")
    updateSelectInput(session, "indicator", choices = data %>% filter(state == region(), series_type == "Seasonally Adjusted") %>% pull(indicator) %>% unique() %>% sort(), selected = "Unemployment rate")
    
    updateCheckboxGroupInput(session, "state", choices = data %>% 
                               filter(indicator == input$indicator, series_type == input$series_type, state != region()) %>% 
                               pull(state) %>% unique() %>% sort(), selected = NULL)

    
  })


  
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  output$plot <- renderPlotly({
    

    
    
    
    df <- data %>% 
      filter(indicator == input$indicator,
             series_type == input$series_type,
             state %in% c(region(), input$state),
             year >= input$date_range,
             gender == "Persons",
             age == "Total (age)") %>%
      group_by(state, date) %>%
      summarise(value = mean(value),
                unit = first(unit)) %>%
      mutate(index = 100*value/value[1]) %>% 
      ungroup()    
    
    validate(
      need(nrow(df) >0, message = FALSE)
    )
    
    if(df$unit[1] == "000") {
    
    p <- ggplot(df, aes(x = date, y = index, colour = state,
                       text = str_c("Region: ", state,
                                    "<br>Date: ", format(date, "%Y"),
                                    "<br>", input$indicator,": ", as_comma(index), ' (Index) ', 
                                    as_comma(df,  group = 'state'), ' (Value)'),
                       group = 1)) + 
      geom_line(size = 0.25) +
      labs(
        x=NULL,
        y= str_c("Index (Base:", month(min(df$date), abbr=F, label = T), year(min(df$date)), "=100)")
      ) +
      scale_colour_aiti() +
      theme(legend.position = 'none')
    } else { p <- ggplot(df, aes(x = date, y = value, colour = state,
                                 text = str_c("Region: ", state,
                                              "<br>Date: ", format(date, "%Y-%b"),
                                              "<br>", input$indicator, ': ', as_percent(value)),
                                 group = 1)) + 
      geom_line(size = 0.25) +
      labs(
        x=NULL,
        y= input$indicator
      ) +
      scale_y_continuous(labels = percent_format(scale = 1, accuracy = 0.1)) +
      scale_colour_aiti() +
      theme(legend.position = 'none')
      
    }
    
    ggplotly(p, tooltip = 'text')
    
  })
}