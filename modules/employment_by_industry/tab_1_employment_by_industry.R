#Employment by industry

empIndUI <- function(id, data) {
  ns <- NS(id)
  
  indicator_choices <- c("Employed total",
                         "Employed full-time",
                         "Employed part-time",
                         "Underemployed total")
  
  industry_choices <- data %>%
    pull(industry) %>%
    unique() %>%
    sort()

  series_choices <- sort(unique(data$series_type))
  date_min <- min(data$date)
  date_max <- max(data$date)
  
  tabPanel(title = uiOutput(ns('title_panel')), plotlyOutput(ns("plot"), width='100%'),
           fluidRow(
             box(status = 'info',solidHeader = TRUE,
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
                 sliderTextInput(
                   inputId = ns('date_range'),
                   label = "Select Date",
                   grid = TRUE,
                   selected = date_max,
                   choices = sort(unique(data$date)))),
             box(status = 'info', solidHeader = TRUE,
                 checkboxGroupInput(
                   inputId = ns('industry'),
                   label = "Select Industry",
                   choices = industry_choices
                 )),
             fluidRow(
               box(width = 12, title = "Download what you see", solidHeader = F,
                   downloadButton(
                     outputId = ns("download_plot"),
                     label = "Download Plot",
                     class = 'download-button'
                   ),
                   downloadButton(
                     outputId = ns("download_data"),
                     label = "Download Data",
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
  
  current_indicator <- reactiveVal(NULL)
  
  observeEvent(input$employment_industry_tab_id, {
    updateSelectInput(session, "indicator", choices = data %>%
                        filter(state == region()) %>%
                        pull(indicator) %>%
                        unique() %>%
                        sort(),
                      selected = "Employed full-time")
  })
  
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
        mutate(share = 100*value/sum(value),
               max_share = ifelse(share == max(share), "fill", 'NA'),
               max_value = ifelse(value == max(value), "fill", 'NA')) %>%
        filter(date == input$date_range) %>%
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
                           fill = ~max_value,
                           text = ~str_c(input$indicator, ": ", as_comma(value),
                                         " (", as_percent(share), ")"))) + 
        geom_bar(stat='identity') + 
        labs(
          y = NULL,
          x = NULL,
          title = str_to_upper(str_c(region(), ": ", input$indicator))
        ) +
        scale_y_continuous(expand = c(0,1), labels = y_labels) +
        scale_fill_manual(values = c(aiti_blue, aiti_grey),
                          breaks = c("fill", "NA")) + 
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
        scale_x_date(expand = c(0,0)) + 
        theme_aiti(legend = 'bottom')
      
    }
    
    ggplotly(p, tooltip = 'text') %>% 
      layout(legend = list(orientation = "h", 
                           y = -0.15))
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
      write.csv(create_data() %>%
                  select(date, industry, value, share), file, row.names = FALSE)
    }
  )
    
    
  
}