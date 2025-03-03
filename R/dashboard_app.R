#'@rawNamespace import(shiny, except = c(column, tabsetPanel, insertTab, actionButton))
#'@rawNamespace import(shinyWidgets, except = c(progressBar))
#'@import fresh
#'@import dplyr
#'@import forcats
#'@import bs4Dash
#'@import ggplot2
#'@import lubridate
#'@import scales
#'@import zoo
#'@import reportabs
#'@import sf
#'@import pkgload
#'@import leaflet
#'@import mapview
#'@import strayr
#'@import pkgload
#'@import sparkline
#'@import formattable
#'@import htmltools
#'@import htmlwidgets
#'@importFrom plotly ggplotly layout plotlyOutput renderPlotly plotly_IMAGE
#'@importFrom stats reorder setNames
#'@importFrom utils download.file tail write.csv
#'@importFrom tidyr tribble
#'
#'@export aiti_dashboard


#### Preamble ####
Sys.setenv("plotly_username" = "hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')





#### Server ####
dash_server <- function(input, output, session) {
  region_selected <- reactive(input$region_select)
  output$region_selected <- renderText({
    region_selected()
  })
  

  #Labour Market -  Tab
  labourMarketServer("lm_ts", data = labour_force)
  labourMarketDemogServer("lm_demog", data = labour_force)
  

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = industry_employment)
  callModule(empIndComparison, "empInd_region", data = industry_employment, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = industry_employment, region = region_selected)
  


  #Dashboard Summary
  table_server("table", data = dashboard_data , region = region_selected)
  
}

aiti_dashboard <- function(...) {
  shinyApp(ui = dash_ui(), server = dash_server)

}
