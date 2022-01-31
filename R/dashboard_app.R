#'@rawNamespace import(shiny, except = c(column, tabsetPanel, insertTab, actionButton))
#'@rawNamespace import(shinyWidgets, except = c(progressBar))
#'@import aitidata
#'@import fresh
#'@import dplyr
#'@import forcats
#'@import ggplot2
#'@import lubridate
#'@import scales
#'@import zoo
#'@import reportabs
#'@import absmapsdata
#'@import sf
#'@import pkgload
#'@import leaflet
#'@import mapview
#'@import strayr
#'@import aititheme
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
# Plotly Setup
Sys.setenv("plotly_username" = "hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')

# phantom_js install
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

# Font Setup
dir.create("~/.fonts")
file.copy("inst/www/Roboto.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')



#### Server ####
dash_server <- function(input, output, session) {
  region_selected <- reactive(input$region_select)
  output$region_selected <- renderText({
    region_selected()
  })
  

  #Labour Market -  Tab
  labourMarketServer("lm_ts", data = aitidata::labour_force)
  labourMarketDemogServer("lm_demog", data = aitidata::labour_force)
  

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = aitidata::employment_by_industry)
  callModule(empIndComparison, "empInd_region", data = aitidata::employment_by_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = aitidata::employment_by_industry, region = region_selected)
  
  #IVI - Tab
  callModule(ivi, "ivi_ts", data = aitidata::internet_vacancies_index, region = region_selected)
  iviComparisonServer("ivi_comparison", data = aitidata::internet_vacancies_index, region = region_selected)
  iviTreeServer("ivi_treemap", data = aitidata::internet_vacancies_index, region = region_selected)
  
  #Weekly payroll data
  covidRegionServer("covid_region", data = aitidata::payroll_index)
  covidDemographicServer("covid_demog", data = aitidata::payroll_index)
  covidIndustryServer("covid_industry", data = aitidata::payroll_index)
  
  #Maps
  map_server("jobkeeper_map", data = aitidata::jobkeeper_sa2)
  map_server("jobseeker_map", data = aitidata::jobseeker_sa2)
  map_server("payroll_map", data = aitidata::payroll_substate)
  map_server("lm_salm", data = aitidata::small_area_labour_market)


  #Employment boxes - row 1
  table_server("table", data = aitidata::labour_force, region = region_selected)
  #JobKeeper Boxes
  # boxes_server("jobkeeper_total", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper applications", percent = FALSE, reverse = TRUE)
  # boxes_server("jobkeeper_proportion", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper proportion", percent = TRUE, reverse = TRUE)
  # 


  #Industry Employment 
  # boxes_server_industry("industry_total", data = aitidata::employment_by_industry, region = region_selected,  "Employed total")
  # boxes_server_industry("industry_ft", data = aitidata::employment_by_industry, region = region_selected, "Employed full-time")
  # boxes_server_industry("industry_pt", data = aitidata::employment_by_industry, region = region_selected, "Employed part-time")
  # boxes_server_industry("industry_under", data = aitidata::employment_by_industry, region = region_selected, "Underemployed total")
  # 

  # callModule(retailTrade, "rt_ts", data = rt)
  # callModule(retailTradeRegional, "rt_region", data  = rt)
  # callModule(retailTradeAnalysis, "rt_analysis", data = rt)
  # callModule(boxes, "turnover_temp", data = rt, indicator = "Turnover", percent = F)
  

  
  # callModule(population, "pop_ts", data = pop)
  # callModule(populationRegion, "pop_region", data = pop)
  # callModule(populationDemo, "pop_demo", data = pop_pyramid)
  # callModule(exports, "exports_ts", data = exports_data)

  # callModule(industry, "industry_ts", data = state_accs)
  # callModule(industryAnalysis, "industry_analysis", data = state_accs)
  # callModule(industryAustralia, "industry_australia", data = nat_accs)
  
}

aiti_dashboard <- function(...) {
  shinyApp(ui = dash_ui(), server = dash_server)

}
