#Employment by industry

empIndUI <- function(id, data) {
  
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Employed part-time",
                         "Underemployed total")
  
  industry_choices <- data %>%
    filter(industry != "Total (industry)") %>%
    pull(industry) %>%
    unique() %>%
    sort()

  series_choices <- sort(unique(data$series_type))

  
  tabPanel(width='100%',
           title = uiOutput(ns('title_panel')), 
           plotlyOutput(ns("plot"), height = "600px"),
           fluidRow(
             box(status = 'info', solidHeader = F,
                 selectInput(
                   inputId = ns("indicator"),
                   label = "Select Indicator",
                   choices = indicator_choices,
                   selected = "Employed Total"),
                 radioButtons(
                   inputId = ns("share"),
                   label = "Display as: ",
                   choices = c("Share", "Value"),
                   selected = "Value"
                 ),
                 uiOutput(ns("date"))
                 ),
             
             box(status = 'info', solidHeader = F,
                 checkboxGroupInput(
                   inputId = ns('industry'),
                   label = "Select Industry (up to 9)",
                   choices = industry_choices
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
             )
           )
  )
}

empInd <- function(input, output, session, data, region) {
  
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  
  output$title_panel <- renderText({
    region()
  })
  
  observe({
    if (length(input$industry) > 9) {
      updateCheckboxGroupInput(session, "industry", selected = tail(input$industry, 9))
    }
  })
  
  
  
  output$date <- renderUI({
    if(is.null(input$industry)) {
      sliderTextInput(
        inputId = session$ns("date"),
        label = "Select Date",
        choices = zoo::as.yearqtr(sort(unique(data$date))),
        selected = zoo::as.yearqtr(date_max))
    }
  })
  

  current_indicator <- reactiveVal(NULL)
  

  observeEvent(region(), {
    
    current_indicator(input$indicator)
    
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region(),
                               indicator != "Underemployment ratio (proportion of employed)") %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(), 
                      selected = "Employed total")
    
  
    
  })
  
  create_data <- reactive({
    if(is.null(input$industry)) {
      df <- data %>%
        filter(industry != "Total (industry)",
               state == region(), 
               indicator == input$indicator) %>% 
        group_by(date, industry) %>% 
        summarise(value = mean(value)) %>% 
        mutate(share = 100*value/sum(value)) %>%
        filter(date == as.Date(zoo::as.yearqtr(input$date)) + months(1)) %>%
        arrange(desc(industry)) %>%
        mutate(industry = as_factor(industry)) 
      } else {
        df <- data %>% 
      filter(state == region(), 
             indicator == input$indicator,
             series_type == "Original",
             gender == "Persons") %>%
      group_by(date) %>%
      mutate(share = 100*value/sum(value)) %>%
      ungroup() %>%
      filter(industry %in% input$industry)
      }
  })
  
  create_plot <- reactive({
    
    validate(
      need(length(input$industry < 10), message = FALSE)
    )
    
    if(input$share == "Share") {
      y_var <- "share"
      y_labels <- percent_format(scale = 1)
    } else {
      y_var <- "value"
      y_labels <- comma_format(scale = 1/1000, suffix = 'k')
    }
    
    if(length(input$industry) > 1) {
      plot_title <- str_to_upper(str_c(region(), ": ", input$indicator, " (Multiple industries)"))
      
    } else {
      plot_title <- str_to_upper(str_c(region(), ": ", input$indicator, " (", input$industry, ")"))
      
    }
    
    if(is.null(input$industry)) {

      
      
      p <- ggplot(create_data(), aes_(x = ~reorder(industry, value), 
                           y =  as.name(y_var),
                           text = ~str_c(input$indicator, ": ", as_comma(value),
                                         " (", as_percent(share), ")"))) + 
        geom_bar(stat='identity', fill = aiti_blue) + 
        labs(
          y = NULL,
          x = NULL,
          title = str_to_upper(str_c(input$indicator, ": ", region(), " (", input$date, ")"))
        ) +
        scale_y_continuous(expand = c(0,0), labels = y_labels) +
        coord_flip() +
        theme_aiti(base_family = "Roboto")
    } else {
      
        p <- ggplot(create_data(), aes_(x = ~date, 
                           y = as.name(y_var),
                           colour = ~industry, 
                           text = ~str_c("Date: ", format(date, "%Y-%b"),
                                         "<br>",industry, ": ", as_comma(value),
                                         " (", as_percent(share), ")"),
                           group = ~industry)) + 
        geom_line() + 
        labs(
          x = NULL, 
          y = NULL,
          title = plot_title
        ) + 
        aititheme::aiti_colour_manual(n = length(input$industry)) +
        scale_y_continuous(labels = y_labels)  +
        theme_aiti(legend = 'bottom', base_family = "Roboto")
      
    }

    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE,
             legend = list(orientation = "h",
                           y = -0.15),
             annotations = list(
               x = 1,
               y = -0.4,
               showarrow = FALSE,
               xref = "paper",
               yref = "paper",
               xanchor = "right",
               yanchor = "auto",
               text = "Source: AITI Economic Indicators"
             ))
  })
  

  output$plot <- renderPlotly({
    validate(
      need(input$date, message = FALSE)
    )

    create_plot()
    
    })
  
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
      write.csv(create_data() %>%
                  select(date, industry, value, share), file, row.names = FALSE)
    }
  )
    
    
  
}