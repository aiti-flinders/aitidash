#Module UI Function
#Labour Market tab/tabPanel

empIndAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  
  
}

empIndAnalysis <- function(input, output, session, data, region) {
  analysis_month <- data %>%
    pull(date) %>%
    max() %>%
    month(abbr = F, label = T)
  
  
  output$analysis <- renderUI({
    
    analysis_text <- str_c(sep=' ', "<h3>", region(), "</h3>",
                           "The ABS Quarterly Labour Force Survey data released in",
                           release(data, ym = "month"),
                           "showed the composition of employment in", region(),".",
                           "<ul><li>Total employment is highest in ",
                           current(data, list(indicator = "Employed total", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),
                           "which employs", 
                           current(data, list(indicator = "Employed total", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </li>",
                           
                           "<li>Full-Time employment is highest in ",
                           current(data, list(indicator = "Employed full-time", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),                           
                           "which employs", 
                           current(data, list(indicator = "Employed full-time", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </li>",

                           "<li>Part-Time employment is highest in ", 
                           current(data, list(indicator = "Employed part-time", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),                           
                           "which employs", 
                           current(data, list(indicator = "Employed part-time", state = region())) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </ul></li>",
                           
                           "<h3>Australia</h3><ul><li>",
                           
                           "In Australia, Total Employment is highest in ",
                           current(data, list(indicator = "Employed total", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),
                           "which employs", 
                           current(data, list(indicator = "Employed total", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </li>",
                           
                           "<li>Full-Time employment is highest in ",
                           current(data, list(indicator = "Employed full-time", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),                           
                           "which employs", 
                           current(data, list(indicator = "Employed full-time", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </li>",
                           
                           "<li>Part-Time employment is highest in ", 
                           current(data, list(indicator = "Employed part-time", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(industry),                           
                           "which employs", 
                           current(data, list(indicator = "Employed part-time", state = "Australia")) %>%
                             filter(value == nth(value, 2, order_by = -value)) %>%
                             pull(value) %>%
                             as_comma(), "people </ul></li>",
                           "<h3>Data</h3><ul><li>
                           Data sourced from ABS Series 6291.0.55.003 table 5. Click <a href = 'https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6291.0.55.003Main+Features1May%202020?OpenDocument' target = '_blank' class = 'button'>here</a> to download.</ul></li>")
    
    
    HTML(analysis_text)
    
    
  })
  
}