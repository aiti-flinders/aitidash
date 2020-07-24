#Module UI Function
#Labour Market tab/tabPanel

labourMarketAnalysisUI <- function(id, data) {
  
  ns <- NS(id)
  
  tabPanel(title = "Analysis", uiOutput(ns("analysis")))
  

  }

labourMarketAnalysis <- function(input, output, session, region, data) {
  
  analysis_text_reports <- str_c(
sep = " ", 
"<h2>Reports</h2>",
"<p>The Australian Industrial Transformation Institute prepares monthly reports on the labour market for all States and Territories.</p>",
"<p>These reports are typically available in the week that the data is released on the ABS website.</p>",
"<p>Each report covers the headline changes to key labour market indicators compared to last month, and over the year. These key indicators
are:<ul>
<li>Total, full-time, and part-time Employment</li>
<li>Unemployment, and Unemployment Rate</li>
<li>Unemployment Rate for Males and Females</li>
<li>Underemployment, and Underemployment Rate</li>
<li>Underutilisation, and Underutilisation Rate</li>
<li>Hours Worked</li></ul>",
"<p>They can be found 
<a href='https://www.flinders.edu.au/australian-industrial-transformation-institute/employment-insights' target = '_blank', class = 'button'>here.</a></p>")
  
  analysis_text_region <- reactive({
    str_c(sep=" ", "<h2>", region(), "</h2>",
          "<ul><li>The ABS Labour Force Survey data for", region(), "released in",
          release(data, 'month', plus = 1L),
          "showed that trend employment",
          change(data, list(indicator = "Employed total", state = region()), "id", "month"), "</li>",
          
          "<li> In aggregate, over the past year, employment in", region(), "has",
          change(data, list(indicator = "Employed total", state = region()), "id", "year"),"</li>", 
          
          "<li> Unemployment in", release(data, "month"), 
          change(data, list(indicator = "Unemployed total", state = region()), "id", "month"),"</li>",
          
          "<li> The unemployment rate", 
          change(data, list(indicator = "Unemployment rate", state = region()), "id", "month"), "</li></ul>")
  })
  
  analysis_text_aus <- reactive({
    str_c(sep = " ", "<h2>Australia</h2><ul><li> Nationally, employment in ", release(data, "month"), 
          change(data, list(indicator = "Employed total", state = "Australia"), "id", "month"),
          
          "<li> Unemployment ", 
          change(data, list(indicator = "Unemployed total", state = "Australia"), "id", "month"), "</li>",
          
          "<li> The unemployment rate", 
          change(data, list(indicator = "Unemployment rate", state = "Australia"), "id", "month"), "</li></ul>")
  })

    

  output$analysis <- renderUI({
    HTML(analysis_text_reports, analysis_text_region())
    
    })
  
}