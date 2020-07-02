#Module UI Function
#Labour Market tab/tabPanel

labourMarketAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  
           
  }

labourMarketAnalysis <- function(input, output, session, region, data) {

  output$analysis <- renderUI({

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
                          

                           "<h3>Data</h3><ul><li> Data sourced from ABS Series 6202.0 tables 12 and 23. Click <a href = ",'www.google.com', " target = '_blank' class = 'button'>here</a> to download.</li></ul>",
                           
                           "<h3>Reports</h3>",
                           "The Australian Industrial Transformation Institute prepares monthly reports on the labour market",
                           "in South Australia, and state-based reports on underutilisation. These reports are typically available",
                           "in the week that the data is released on the ABS website.",
                           "<div class = 'row'><div class = 'column'>",
                           "<h4>Underutilisation</h4>",
                           "<ul><li>Australia</li>",
                           "<li>Australian Capital Territory</li>",
                           "<li>New South Wales</li>",
                           "<li>Northern Territory</li>",
                           "<li>Queensland</li>",
                           "<li>South Australia</li>",
                           "<li>Tasmania</li>",
                           "<li>Victoria</li>",
                           "<li>Western Australia</li></ul></div>",
                           "<div class = 'column'>",
                           "<h4>Labour Market Reports</h4>",
                           "<ul><li>May</li>",
                           "<li>April</li>",
                           "<li>March</li></ul></div></div>"
                           )
                           
                          
    HTML(analysis_text)
    
    
})
  
}