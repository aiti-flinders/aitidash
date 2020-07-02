#retail_trade_analysis
#Module UI Function
#Labour Market tab/tabPanel

retailTradeAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  
}

retailTradeAnalysis <- function(input, output, session, data) {
  analysis_month <- data %>%
    pull(date) %>%
    max() %>%
    month(abbr = F, label = T)
  
  
  output$analysis <- renderUI({
    
    analysis_text <- str_c(sep=' ', "<h3>South Australia</h3><ul><li>",
                           "The ABS Retail Trade Survey released in",
                           release_month(data),
                           "showed that total retail turnover in South Australia",
                           incr_decr(data, "month", "Turnover", "Trend"), "by", 
                           last_month(data, "Turnover", "South Australia",  "Trend")['num'] %>% 
                             formatC(digits = 0, format = 'd', big.mark =','), "or",
                           last_month(data, "Turnover", "South Australia",  "Trend")['pct'] %>%
                             formatC(digits = 1, format = 'f'), "% to", 
                           current(data, "Turnover", "South Australia",  "Trend") %>%
                             formatC(digits = 0, format = 'd', big.mark = ','), "</li></ul>")
                           
                          
    HTML(analysis_text)
    
  })
}