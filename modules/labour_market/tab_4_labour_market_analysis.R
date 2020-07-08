#Module UI Function
#Labour Market tab/tabPanel

labourMarketAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  
           
  }

labourMarketAnalysis <- function(input, output, session, region, data) {
  
  analysis_text <- reactive({
    analysis_text <- str_c(sep=" ", "<h3>", region(), "</h3><ul><li>",
                         "The ABS Labour Force Survey data for", region(), "released in",
                         release(data, 'month', plus = 1L),
                         "showed that trend employment",
                         change(data, list(indicator = "Employed total", state = region()), "id", "month"), "</li>",
                         
                         "<li> In aggregate, over the past year, employment in", region(), "has",
                         change(data, list(indicator = "Employed total", state = region()), "id", "year"),"</li>", 
                         
                         "<li> Unemployment in", release(data, "month"), 
                         change(data, list(indicator = "Unemployed total", state = region()), "id", "month"),"</li>",
                         
                         "<li> The unemployment rate", 
                         change(data, list(indicator = "Unemployment rate", state = region()), "id", "month"), "</li></ul>",
                         
                         "<h3>Australia</h3><ul><li> Nationally, employment in ", release(data, "month"), 
                         change(data, list(indicator = "Employed total", state = "Australia"), "id", "month"),
                         
                         "<li> Unemployment ", 
                         change(data, list(indicator = "Unemployed total", state = "Australia"), "id", "month"), "</li>",
                         
                         "<li> The unemployment rate", 
                         change(data, list(indicator = "Unemployment rate", state = "Australia"), "id", "month"), "</li></ul>",
                         
                         
                         "<h3>Data</h3><ul><li> Data sourced from ABS Series 6202.0 tables 12 and 23. Click <a href = 'https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6202.0May%202020?OpenDocument' target = '_blank' class = 'button'>here</a> to download.</li></ul>",
                         
                         "<h3>Reports</h3>",
                         "The Australian Industrial Transformation Institute prepares monthly reports on the labour market",
                         "for all States and Territories. These reports are typically available",
                         "in the week that the data is released on the ABS website. They can be found ",
                         "<a href='https://www.flinders.edu.au/australian-industrial-transformation-institute/employment-insights'>
                           here</a>"
  )})

  output$analysis <- renderUI({

    HTML(analysis_text())
    
    
})
  
}