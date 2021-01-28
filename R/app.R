#'@import shiny
#'@import shinydashboard
#'@import shinyWidgets
#'@import shinycssloaders
#'@import dplyr
#'@importFrom plotly ggplotly layout plotlyOutput renderPlotly plotly_IMAGE
#'@import ggplot2
#'@import forcats
#'@import scales
#'@import zoo
#'@import reportabs
#'@import absmapsdata
#'@import sf
#'@import leaflet
#'@import mapview
#'@import strayr
#'@import lubridate
#'@import aititheme

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
  title = "Economic Indicators Dashboard",
  tags$li(
    #a(href = "http://www.flinders.edu.au/aiti",
      img(
        src = "custom-assets/aiti_logo.png", 
        title = "Australian Industrial Transformation Institute", 
        height = "17px"
        ),
    class = "dropdown")
    )


##### Sidebar Controls #####
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  width = '230px',    
  sidebarUI("sidebar")
)

#### User Guide ####
user_guide_tab <- tabItem(
  tabName = "user_guide",
  fluidRow(
    tabBox(
      id = "user_guide_tab_id",
      width = 12,
      userGuideUI("user_guide"),
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
  tabName = 'labour_market',
  fluidRow(
    tabBox(
      id = 'labour_market_tab_id',
      width = 12,
      labourMarketUI("lm_ts", data = aitidata::labour_force),
      labourMarketRegionalUI("lm_region", data = aitidata::labour_force),
      labourMarketDemogUI("lm_demog", data = aitidata::labour_force),
      labourMarketSmallAreaUI("lm_salm", data = aitidata::small_area_labour_market)
      #labourMarketAnalysisUI("lm_analysis", data = aitidata::labour_force)
    )
  )
)

#Employment by Industry
emp_ind_tab <- tabItem(
  tabName = 'industry',
  fluidRow(
    tabBox(
      id = "employment_industry_tab_id",
      width = 12,
      empIndUI("empInd_ts", data = aitidata::employment_by_industry),
      empIndComparisonUI("empInd_region", data = aitidata::employment_by_industry)
      #empIndAnalysisUI("empInd_analysis", data = employment_industry)
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

#COVID Tab
covid_map_tab <- tabItem(
  tabName = "covid_map",
  fluidRow(
    tabBox(
      id = "covid_map_tab_id",
      width = 12,
      covidUI("covid_map", data = aiti::covid_data)
    )
  )
)

covid_payroll_tab <- tabItem(
  tabName = "covid_payroll",
  fluidRow(
    tabBox(
      id = "covid_payroll_tab_id",
      width = 12,
      covidRegionUI("covid_region", data = aitidata::payroll_index),
      covidDemographicUI("covid_demog", data = aitidata::payroll_index),
      covidIndustryUI("covid_industry", data = aitidata::payroll_index)
      
    )
  )
)
                         
#### Body ####                                       
body <- dashboardBody(
  #tags$head(includeHTML(("custom-assets/google-analytics.html"))),
  tags$head(tags$link(rel = "stylesheet", type = 'text/css', href = 'custom-assets/custom.css')),
  tabItems(
    user_guide_tab,
    dashboard_tab,
    labour_market_tab,
    emp_ind_tab,
    internet_vacancies_tab,
    covid_map_tab,
    covid_payroll_tab 
  )
)
  
#### UI ####
ui <- dashboardPage(
  title = "Economic Indicators Dashboard",
  header,
  sidebar,
  body
)

#### Server ####
server <- function(input, output) {
  region_selected <- reactive(input$region_select)
  output$region_selected <- renderText({
    region_selected()
  })
  
  #Labour Market -  Tab
  labourMarketServer("lm_ts", data = aitidata::labour_force, region = region_selected)
  labourMarketRegionalServer("lm_region", data = aitidata::labour_force, region = region_selected)
  labourMarketDemogServer("lm_demog", data = aitidata::labour_force, region = region_selected)
  labourMarketSmallAreaServer("lm_salm", data = aitidata::small_area_labour_market, region = region_selected)
  #callModule(labourMarketAnalysis, "lm_analysis", data = aitidata::labour_force, region = region_selected)

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = aitidata::employment_by_industry, region = region_selected)
  callModule(empIndComparison, "empInd_region", data = aitidata::employment_by_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = aitidata::employment_by_industry, region = region_selected)
  
  #IVI - Tab
  callModule(ivi, "ivi_ts", data = aitidata::internet_vacancies_index, region = region_selected)
  iviComparisonServer("ivi_comparison", data = aitidata::internet_vacancies_index, region = region_selected)
  iviTreeServer("ivi_treemap", data = aitidata::internet_vacancies_index, region = region_selected)
  
  #COVID - Tab
  covidServer("covid_map", data = aiti::covid_data, region = region_selected)
  covidRegionServer("covid_region", data = aitidata::payroll_index, region = region_selected)
  covidDemographicServer("covid_demog", data = aitidata::payroll_index, region = region_selected)
  covidIndustryServer("covid_industry", data = aitidata::payroll_index, region = region_selected)

  #Employment boxes - row 1
  boxesServer("employment_total", data = aitidata::labour_force, region = region_selected, "Employed total", reverse = F, percent = F)
  boxesServer("employment_ft", data = aitidata::labour_force, region = region_selected, "Employed full-time", reverse = F, percent = F)
  boxesServer("employment_pt", data = aitidata::labour_force, region = region_selected, "Employed part-time", reverse = F, percent = F)

  #Labour underutilisation (number) boxes - row 2
  boxesServer("unemployed", data = aitidata::labour_force, region = region_selected, "Unemployed total", reverse = T, percent = F)
  boxesServer("underemployed", data = aitidata::labour_force, region = region_selected, "Underemployed total", reverse = T, percent = F)
  boxesServer("underutilised", data = aitidata::labour_force, region = region_selected, "Underutilised total", reverse = T, percent = F)
  
  #Labour underutilisation rates - row 3
  boxesServer("unemployment_rate", data = aitidata::labour_force, region = region_selected, "Unemployment rate", reverse = T, percent = T)
  boxesServer("underemployment_rate", data = aitidata::labour_force, region = region_selected, "Underemployment rate (proportion of labour force)", reverse = T, percent = T)
  boxesServer("underutilisation_rate", data = aitidata::labour_force, region = region_selected, "Underutilisation rate", reverse = T, percent = T)
  
  
  #Hours worked 
  boxesServer("hours_worked_total", data = aitidata::labour_force, region = region_selected, "Monthly hours worked in all jobs")
  boxesServer("participation_rate", data = aitidata::labour_force, region = region_selected, "Participation rate", percent = TRUE)
  boxesServer("labour_force_total", data = aitidata::labour_force, region = region_selected, "Labour force total")
  
  #JobKeeper Boxes
  boxesServer("jobkeeper_total", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper applications", percent = FALSE, reverse = TRUE)
  boxesServer("jobkeeper_proportion", data = aitidata::jobkeeper_state, region = region_selected, "Jobkeeper proportion", percent = TRUE, reverse = TRUE)
  
  #Jobseeker Boxes
  #boxesServer("jobseeker_total", data = jobseeker_state, region = region_selected, "Jobseeker payment", percent = FALSE, reverse = TRUE)
  #boxesServer("ya_total", data = jobseeker_state, region = region_selected, "Youth allowance other", percent = FALSE, reverse = TRUE)
  
  

  #Industry Employment 
  boxesAltServer("industry_total", data = aitidata::employment_by_industry, region = region_selected,  "Employed total")
  boxesAltServer("industry_ft", data = aitidata::employment_by_industry, region = region_selected, "Employed full-time")
  boxesAltServer("industry_pt", data = aitidata::employment_by_industry, region = region_selected, "Employed part-time")
  boxesAltServer("industry_under", data = aitidata::employment_by_industry, region = region_selected, "Underemployed total")
  

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