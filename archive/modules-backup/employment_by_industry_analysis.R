#Module UI Function
#Labour Market tab/tabPanel

empIndAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  
  
}

empIndAnalysis <- function(input, output, session, data) {
  analysis_month <- data %>%
    pull(date) %>%
    max() %>%
    month(abbr = F, label = T)
  
  
  output$analysis <- renderUI({
    
    analysis_text <- str_c(sep=' ', "<h3>South Australia</h3>",
                           "The ABS Quarterly Labour Force Survey released in",
                           release_month(data),
                           "showed the composition of South Australia's employment.",
                           "<ul><li>Total employment is highest in ",
                           employment_industry(data, "Employed Total", "South Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Total", region_name = "South Australia")$value, "people"), "</li>",
                           
                           "<li>Full-Time employment is highest in ",
                           employment_industry(data,series_name = "Employed Full-Time", region_name = "South Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Full-Time", region_name = "South Australia")$value, "people"), "</li>", 

                           "<li>Part-Time employment is highest in ", 
                           employment_industry(data,series_name = "Employed Part-Time", region_name = "South Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Part-Time", region_name = "South Australia")$value, "people"), "</ul></li>",
                           
                           "<h3>Australia</h3><ul><li>",
                           "In Australia, Total Employment is highest in ",
                           employment_industry(data,series_name = "Employed Total", region_name = "Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Total", region_name = "Australia")$value, "people"), "</li>",
                           
                           "<li>Full-Time employment is highest in ",
                           employment_industry(data,series_name = "Employed Full-Time", region_name = "Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Full-Time", region_name = "Australia")$value, "people"), "</li>",
                           
                           "<li>Part-Time employment is highest in ", 
                           employment_industry(data,series_name = "Employed Part-Time", region_name = "Australia")$industry, 
                           "which employs", as_comma(employment_industry(data,series_name = "Employed Part-Time", region_name = "Australia")$value, "people"), "</ul></li>",
                           
                           "<h3>Data</h3><ul><li>
                           Data sourced from ABS Series 6291.0.55.003 table 5. Click <a href = ",abs_url('6291.0.55.003', 2019, 2), " target = '_blank' class = 'button'>here</a> to download.</ul></li>")
    
    
    HTML(analysis_text)
    
    
  })
  
}