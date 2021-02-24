#'@rawNamespace import(shiny, except = c(insertTab, actionButton, tabsetPanel, column))
#'@rawNamespace import(shinyWidgets, except = c(progressBar))
#'@import bs4Dash
#'@import aitidata
#'@import fresh
#'@import shinycssloaders
#'@import dplyr
#'@import forcats
#'@import ggplot2
#'@import lubridate
#'@import scales
#'@import zoo
#'@import reportabs
#'@import absmapsdata
#'@import sf
#'@import leaflet
#'@import mapview
#'@import strayr
#'@import aititheme
#'@importFrom plotly ggplotly layout plotlyOutput renderPlotly plotly_IMAGE
#'@importFrom stats reorder setNames
#'@importFrom utils download.file tail write.csv
#'@importFrom purrr pmap
#'@importFrom tidyr tribble

aitidashboard <- function(...) {

#### Preamble ####
# Plotly Setup
Sys.setenv("plotly_username" = "hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')

# phantom_js install
if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }

# Font Setup
dir.create("~/.fonts")
file.copy("www/Roboto.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')


#### Header Controls ####
header <- dashboardHeader(
  skin = "light",
  title = dashboardBrand(
    title = "Economic Indicators",
    href = "http://www.flinders.edu.au/aiti",
    image =  "custom-assets/aiti_logo.png",
    opacity = 1.0
  ),
  fixed = TRUE,
  leftUI = conditionalPanel(
    condition = "input.sidebarmenu === 'dashboard'",
    radioGroupButtons(
      inputId = "region_select",
      choiceNames = toupper(strayr(regions())),
      choiceValues = regions(),
      selected = "Australia",
      direction = "horizontal",
      justified = FALSE
    )
  )
)


##### Sidebar Controls #####
sidebar <- dashboardSidebar(
  skin = "light",
  width = "230px",
  sidebar("sidebar")
)

#### User Guide ####
user_guide_tab <- tabItem(
  tabName = "user_guide",
  fluidRow(
    tabBox(
      id = "user_guide_tab_id",
      width = 12,
      user_guide("user_guide"),
      specificInstructionsUI("specific_instructions")
    )
  )
)

#### Dashboard Tab ####
dashboard_tab <- tabItem(
  tabName = 'dashboard',
  dashboardUI("dashboard")
)

#### Labour Market Tab ####
labour_market_tab <- tabItem(
  tabName = 'employment',
  fluidRow(
    tabBox(
      id = 'labour_market_tab_id',
      width = 12,
      labourMarketUI("lm_ts", data = aitidata::labour_force),
      labourMarketDemogUI("lm_demog", data = aitidata::labour_force)
    )
  )
)

jobs_payroll_tab <- tabItem(
  tabName = "jobs_payroll",
  fluidRow(
    tabBox(
      id = "jobs_payroll_tab_id",
      width = 12,
      covidRegionUI("covid_region", data = aitidata::payroll_index),
      covidDemographicUI("covid_demog", data = aitidata::payroll_index)
    )
  )
)

#Employment by Industry
emp_ind_tab <- tabItem(
  tabName = 'industry_employment',
  fluidRow(
    tabBox(
      id = "employment_industry_tab_id",
      width = 12,
      empIndUI("empInd_ts", data = aitidata::employment_by_industry),
      empIndComparisonUI("empInd_region", data = aitidata::employment_by_industry)
    )
  )
)

industry_payroll_tab <- tabItem(
  tabName = "industry_payroll",
  fluidRow(
    tabBox(
      id = "industry_payroll_tab_id",
      width = 12,
      covidIndustryUI("covid_industry", data = aitidata::payroll_index)
      
    )
  )
)



#IVI Tab
internet_vacancies_tab <- tabItem(
  tabName = "internet_vacancies", 
  fluidRow(
    tabBox(
      id = "internet_vacancies_tab_id",
      width = 12,
      iviUI("ivi_ts", data = aitidata::internet_vacancies_index),
      iviComparisonUI("ivi_comparison", data = aitidata::internet_vacancies_index),
      iviTreeUI("ivi_treemap", data = aitidata::internet_vacancies_index)
    )
  )
)

#### Map Tabs ####
map_tab <- tabItem(
  tabName = "maps",
  fluidRow(
    tabBox(
      id = "map_tab_id",
      width = 12,
      map_ui("lm_salm", data = aitidata::small_area_labour_market, title = "Small Area Labour Market"),
      map_ui("jobseeker_map", data = aitidata::jobseeker_sa2, title = "JobSeeker Data"),
      map_ui("jobkeeper_map", data = aitidata::jobkeeper_sa2, title = "JobKeeper Data"),
      map_ui("payroll_map", data = aitidata::payroll_substate, title = "Weekly Payroll Data")
      
    )
  )
)
                         
#### Body ####                                       
body <- dashboardBody(
  use_theme(mytheme()),
  #tags$head(includeHTML(("custom-assets/google-analytics.html"))),
  tags$head(tags$link(rel = "stylesheet", type = 'text/css', href = 'custom-assets/custom2.css')),
  tabItems(
    dashboard_tab,
    labour_market_tab,
    jobs_payroll_tab,
    emp_ind_tab,
    industry_payroll_tab,
    internet_vacancies_tab,
    map_tab
  )
)
  
#### UI ####
ui <- dashboardPage(
  dark = FALSE,
  title = "Economic Indicators Dashboard",
  header = header,
  sidebar = sidebar,
  body = body, 
  footer = dashboardFooter(left ="Australian Industrial Transformation Institute", fixed = TRUE, right = "Flinders University")
)

#### Server ####
server <- function(input, output) {
  region_selected <- reactive(input$region_select)
  output$region_selected <- renderText({
    region_selected()
  })
  

  #Labour Market -  Tab
  labourMarketServer("lm_ts", data = aitidata::labour_force)
  labourMarketDemogServer("lm_demog", data = aitidata::labour_force)
  

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = aitidata::employment_by_industry, region = region_selected)
  callModule(empIndComparison, "empInd_region", data = aitidata::employment_by_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = aitidata::employment_by_industry, region = region_selected)
  
  #IVI - Tab
  callModule(ivi, "ivi_ts", data = aitidata::internet_vacancies_index, region = region_selected)
  iviComparisonServer("ivi_comparison", data = aitidata::internet_vacancies_index, region = region_selected)
  iviTreeServer("ivi_treemap", data = aitidata::internet_vacancies_index, region = region_selected)
  
  #Maps
  map_server("jobkeeper_map", data = aitidata::jobkeeper_sa2)
  map_server("jobseeker_map", data = aitidata::jobseeker_sa2)
  map_server("payroll_map", data = aitidata::payroll_substate)
  map_server("lm_salm", data = aitidata::small_area_labour_market)
  covidRegionServer("covid_region", data = aitidata::payroll_index, region = region_selected)
  covidDemographicServer("covid_demog", data = aitidata::payroll_index, region = region_selected)
  covidIndustryServer("covid_industry", data = aitidata::payroll_index, region = region_selected)

  #Employment boxes - row 1
  pmap(box_map(), function(id, indicator, reverse, percent, ...) boxes_server(id, aitidata::labour_force, indicator, region_selected, reverse, percent))
                                                                                       
  #JobKeeper Boxes
  boxes_server("jobkeeper_total", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper applications", percent = FALSE, reverse = TRUE)
  boxes_server("jobkeeper_proportion", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper proportion", percent = TRUE, reverse = TRUE)



  #Industry Employment 
  boxes_server_industry("industry_total", data = aitidata::employment_by_industry, region = region_selected,  "Employed total")
  boxes_server_industry("industry_ft", data = aitidata::employment_by_industry, region = region_selected, "Employed full-time")
  boxes_server_industry("industry_pt", data = aitidata::employment_by_industry, region = region_selected, "Employed part-time")
  boxes_server_industry("industry_under", data = aitidata::employment_by_industry, region = region_selected, "Underemployed total")
  

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

shinyApp(ui = ui, server = server)


}