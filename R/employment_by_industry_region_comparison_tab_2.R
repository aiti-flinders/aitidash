#Employment by industry

empIndComparisonUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Employed part-time")
  
  series_choices <- "Original"
  region_choices <- sort(unique(data$state))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = "Regional Comparison", plotlyOutput(ns("plot"), width='100%', height = "600px"),
           fluidRow(
             box(status = 'info',solidHeader = F,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator", 
                   choices = indicator_choices),
                 sliderTextInput(
                   inputId = ns("date"),
                   label = "Select Date",
                   choices = zoo::as.yearqtr(sort(unique(data$date))),
                   selected = zoo::as.yearqtr(date_max)
                 )),
             box(status='info',solidHeader = F,
                 radioButtons(
                   inputId = ns("comparison"),
                   label = "Select Comparison Region",
                   choices = region_choices)
             )),
           fluidRow(
             box(width = 12, status = "info", title = "Downloads", solidHeader = F,
                 downloadButton(
                   outputId = ns("download_plot"),
                   label = "Click here to download the chart as a .png",
                   class = 'download-button'
                 ),
                 downloadButton(
                   outputId = ns("download_data"),
                   label = "Click here to download the chart data",
                   class = 'download-button'
                 ))
           ))
}

empIndComparison <- function(input, output, session, data, region) {
  
  current_selection <- reactiveVal(NULL)
  
  observeEvent(region(), {
    updateRadioButtons(session, "comparison", choices = data %>% 
                         filter(state != region()) %>% 
                         pull(state) %>%
                         unique() %>%
                         sort(),
                       selected = current_selection())
  })
  
  create_data <- reactive({
    df <- data %>%
      filter(indicator == input$indicator,
             industry != "Total (industry)") %>% 
      group_by(date, state, industry) %>% 
      summarise(value = mean(value)) %>% 
      mutate(share = 100*value/sum(value)) %>%
      ungroup() %>%
      filter(state %in% c(region(), input$comparison),
             date == as.Date(zoo::as.yearqtr(input$date)) + months(1)) %>%
      select(-value) %>% 
      tidyr::pivot_wider(names_from = state, values_from = share) %>%
      arrange(!!as.name(region())) %>%
      mutate(industry = forcats::as_factor(industry)) 
  })
  
  create_plot <- reactive({
    p <- ggplot(create_data()) + 
      geom_segment(aes(x = industry, 
                       xend = industry, 
                       y = !!as.name(region()), 
                       yend = !!as.name(input$comparison)), 
                   colour = aiti_darkblue) + 
      geom_point(aes(x = industry, 
                     y = !!as.name(region()),
                     text = paste0(region(),
                                  "<br>",industry, 
                                  "<br>", as_percent(!!as.name(region()))),
                     fill = region()),
                 shape = 21, 
                 colour = aiti_yellow) +
      geom_point(aes(x = industry, 
                     y = !!as.name(input$comparison),
                     text = paste0(input$comparison, 
                                  "<br>", industry,
                                  "<br>", as_percent(!!as.name(input$comparison))),
                     fill = input$comparison), 
                 shape = 21,
                 colour = aiti_blue) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      scale_fill_manual(breaks = c(region(), input$comparison), values = c(aiti_yellow, aiti_blue)) + 
      coord_flip() + 
      labs(
        y = NULL,
        x = NULL,
        title = toupper(paste0("share of industry employment: ", region(), " & ", input$comparison, " (", input$date, ")"))
      ) +
      theme_aiti(legend = 'bottom', base_family = "Roboto")
    
    
    ggplotly(p, tooltip = 'text') %>%
      layout(autosize = TRUE,
             legend = list(orientation = "h",
                           y = -0.15),
             annotations = list(
               x = 1,
               y = -0.2,
               showarrow = FALSE,
               xref = "paper",
               yref = "paper",
               xanchor = "right",
               yanchor = "auto",
               text = "Source: AITI Economic Indicators"
             ))
  })
  
  output$plot <- renderPlotly({create_plot()})
  
  output$download_plot <- downloadHandler(
    filename = function(){
      paste(input$indicator, "-plot.png", sep = '')
    },
    content = function(file) {
      plotly_IMAGE(create_plot(), out_file = file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$indicator, "-data.csv", sep = '')
    },
    content = function(file) {
      write.csv(create_data(), file, row.names = FALSE)
    }
  )
}