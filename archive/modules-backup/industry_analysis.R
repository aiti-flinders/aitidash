industryAnalysisUI <- function(id, data) {
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns('analysis')))
}

industryAnalysis <- function(input, output, session, data) {
  
  analysis_month <- data %>%
    pull(date) %>%
    max() %>%
    month(abbr = F, label = T)
  
  
  output$analysis <- renderUI({
    
    analysis_text <- str_c(sep=' ', "<h3>South Australia</h3><ul><li>",
                           "The ABS State National Accounts, current to",
                           release_month(data),
                           "showed that Gross State Product in South Australia",
                           incr_decr(data, "year", "Gross State Product", s_type = "Original"), "by", 
                           as_comma(suffix = 'dollars', last_year(data, "Gross State Product",  s_type = "Original")['num']), "or",
                           as_percent(last_year(data, "Gross State Product",  s_type =  "Original")['pct']), "to", 
                           as_comma(suffix = 'dollars',current(data, "Gross State Product", s_type =  "Original")), "</li></ul>",
                           
                          
                           
                           
                           
                           "<h3>Data</h3><ul><li> South Australian data sourced from ABS Series 5220.0 Table 5. Click <a href = ",abs_url('5220.0', 2019, 6), " target = '_blank' class = 'button'>here</a> to download.</ul></li>"
    )
    
    
    HTML(analysis_text)
  })
  
}