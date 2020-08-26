library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(reportabs)
library(daitir)
library(absmapsdata)
library(sf)
library(aititheme)
library(leaflet)
library(mapview)

#### Preamble ####
# Plotly Setup
Sys.setenv("plotly_username" = "hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')


# Font Setup
dir.create("~/.fonts")
file.copy("www/Roboto.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')



#### Header Controls ####

header <- dashboardHeader(#titleWidth = "200px",
                          title = "Economic Indicators Dashboard",
                          tags$li(a(href = "http://www.flinders.edu.au/aiti",
                                    img(src = "aiti_logo.png",
                                        title = "Australian Industrial Transformation Institute", height = "17px")),
                                  
                                    class = "dropdown"))

##### Sidebar Controls #####
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  width = '230px',
  sidebarMenu(
    id = 'tabs',
    menuItem(
      text = "Summary",
      tabName = 'dashboard',
      icon = icon("chart-bar"),
      selected = TRUE
    ),   
    menuItem(
      text = "User Guide",
      tabName = "user_guide",
      icon = icon("book-reader")
    ),
    menuItem(
      text = "Employment Insights",
      tabName = "employment",
      icon = icon("briefcase"),
      menuItem(text = "Labour Force",
                  tabName = "labour_market",
                  icon = icon("chart-line")),
      menuItem(text = "Internet Vacancies",
                  tabName = "internet_vacancies",
                  icon = icon("newspaper"))),
    menuItem(
      text = "Industry Insights",
      tabName = "industry_insights",
      icon = icon('industry'),
      menuItem(text = "Industry Employment",
                  tabName = "industry",
                  icon = icon("city"))),
      # menuItem(text = "Industry Value Add",
      #          tabName = "industry_va",
      #          icon = icon("plus"))),
    # menuItem(
    #   text = "Economic Complexity",
    #   tabName = "economic_complexity",
    #   icon = icon("cog"),
    #   menuItem(text = "Product Indicators",
    #            tabName = "ec_products",
    #            icon = icon("cube")),
    #   menuItem(text = "Country Indicators",
    #            tabName = "ec_countries",
    #            icon = icon("globe-asia"))
    # ),
    menuItem(
      text = "COVID-19",
      tabName = "covid_19",
      icon = icon("virus"), 
      menuItem(text = "Maps",
               tabName = "covid_map",
               icon = icon("map"),
               badgeLabel = "new!"
               ),
      menuItem(text = "Payroll Jobs",
               tabName = "covid_payroll",
               icon = icon("chart-line")
               )
    ),
    
    radioButtons(
      inputId = "region_select",
      label = "Select Region",
      choices = c("Australia","New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory"),
      selected = "Australia"
    )
  )
)



#### User Guide ####
user_guide_tab <- tabItem(
  tabName = "user_guide",
  fluidRow(
    tabBox(id = "user_guide_tab_id",
           width = 12,
           userGuideUI("user_guide"),
           specificInstructionsUI("specific_instructions")
    )
)
)
lf_next_release <- abs_next_release("6202.0")
lf_release_date <- abs_release_date("6202.0")
industry_next_release <- abs_next_release("6291.0.55.003")
industry_release_date <- abs_release_date("6291.0.55.003")
#### Dashboard Tab ####
dashboard_tab <- tabItem(
  tabName = 'dashboard',
  # fluidPage(width = 12,
  #           DashboardUI("dashboard"))
  h2("Employment Insights"),
  h2(textOutput("region_selected")),
  p(paste0("This data is current as of ", release(labour_force, "month"), " ", release(labour_force, "year"),".")),
  p(paste0("Data for ",  
          lf_next_release, 
          " will be available on ", 
          weekdays(lf_release_date), ", the ", day(lf_release_date), "th of ", month(lf_release_date, abbr = F, label = T), ".")),
  fluidRow( 
    boxesUI('unemployment_rate'),    
    boxesUI('unemployed')
  ),
  fluidRow(
    boxesUI('employment_total'), 
    boxesUI('participation_rate')
  ),
  fluidRow(
    boxesUI('employment_ft'),
    boxesUI('employment_pt')
  ),
  fluidRow(    
    boxesUI('underemployed'),
    boxesUI('underutilised')

  ),
  fluidRow(    
    boxesUI('underemployment_rate'),
    boxesUI('underutilisation_rate')
  ),
  fluidRow(
    boxesUI('hours_worked_total'),
    boxesUI('labour_force_total')
  ),
  h2("Industry Insights"),
  HTML(
    paste0("The boxes below show which industry employs the most people overall, full-time, and part-time. 
          Each box displays the industry name, the number of people employed, and the yearly change. ")
  ),
  p(paste0("This data is current as at: ", release(employment_industry, "month"), " ", release(employment_industry, "year"))),
  p(paste0("Data for ", 
          industry_next_release, 
          " will be available on ", 
          weekdays(industry_release_date), ", the ", day(industry_release_date), "th of ", month(industry_release_date, abbr = F, label = T), ".")),
    
  fluidRow(
    boxesAltUI("industry_total"),
    boxesAltUI("industry_ft")

  ),
  fluidRow(
    boxesAltUI("industry_pt"),
    boxesAltUI("industry_under")
  )
)

                     


#### Labour Market Tab ####
labour_market_tab <- tabItem(
  tabName = 'labour_market',
  fluidRow(
    tabBox(id = 'labour_market_tab_id',
           width = 12,
           labourMarketUI("lm_ts", data = labour_force),
           labourMarketRegionalUI("lm_region", data = labour_force),
           labourMarketDemogUI("lm_demog", data = labour_force)
           #labourMarketSmallAreaUI("lm_salm", data = small_area_labour_market),
           #labourMarketAnalysisUI("lm_analysis", data = labour_force)
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
      empIndUI("empInd_ts", data = employment_industry),
      empIndComparisonUI("empInd_region", data = employment_industry)
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
      iviUI("ivi_ts", data = internet_vacancies_index),
      iviComparisonUI("ivi_comparison", data = internet_vacancies_index),
      iviTreeUI("ivi_treemap", data = internet_vacancies_index)
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
      covidUI("covid_map", data = covid_data)
    )
  )
)

covid_payroll_tab <- tabItem(
  tabName = "covid_payroll",
  fluidRow(
    tabBox(
      id = "covid_payroll_tab_id",
      width = 12,
      covidRegionUI("covid_region", data = payroll_index),
      covidDemographicUI("covid_demog", data = payroll_index),
      covidIndustryUI("covid_industry", data = payroll_index)
      
    )
  )
)
                         
#### Body ####                                       
body <- dashboardBody(
  tags$head(includeHTML(("www/google-analytics.html"))),
  tags$head(tags$link(rel = "stylesheet", type = 'text/css', href = 'custom.css')),
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
ui <- dashboardPage(title = "Economic Indicators Dashboard",
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
  labourMarketServer("lm_ts", data = labour_force, region = region_selected)
  labourMarketRegionalServer("lm_region", data = labour_force, region = region_selected)
  labourMarketDemogServer("lm_demog", data = labour_force, region = region_selected)
  # callModule(labourMarketSmallArea, "lm_salm", data = small_area_labour_market, region = region_selected)
  #callModule(labourMarketAnalysis, "lm_analysis", data = labour_force, region = region_selected)

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = employment_industry, region = region_selected)
  callModule(empIndComparison, "empInd_region", data = employment_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = employment_industry, region = region_selected)
  
  #IVI - Tab
  callModule(ivi, "ivi_ts", data = internet_vacancies_index, region = region_selected)
  iviComparisonServer("ivi_comparison", data = internet_vacancies_index, region = region_selected)
  iviTreeServer("ivi_treemap", data = internet_vacancies_index, region = region_selected)
  
  #COVID - Tab
  covidServer("covid_map", data = covid_data, region = region_selected)
  covidRegionServer("covid_region", data = payroll_index, region = region_selected)
  covidDemographicServer("covid_demog", data = payroll_index, region = region_selected)
  covidIndustryServer("covid_industry", data = payroll_index, region = region_selected)

  #Employment boxes - row 1
  callModule(boxes, "employment_total", data = labour_force, region = region_selected, "Employed total", reverse = F, percent = F)
  callModule(boxes, "employment_ft", data = labour_force, region = region_selected, "Employed full-time", reverse = F, percent = F)
  callModule(boxes, "employment_pt", data = labour_force, region = region_selected, "Employed part-time", reverse = F, percent = F)

  #Labour underutilisation (number) boxes - row 2
  callModule(boxes, "unemployed", data = labour_force, region = region_selected, "Unemployed total", reverse = T, percent = F)
  callModule(boxes, "underemployed", data = labour_force, region = region_selected, "Underemployed total", reverse = T, percent = F)
  callModule(boxes, "underutilised", data = labour_force, region = region_selected, "Underutilised total", reverse = T, percent = F)
  
  #Labour underutilisation rates - row 3
  callModule(boxes, "unemployment_rate", data = labour_force, region = region_selected, "Unemployment rate", reverse = T, percent = T)
  callModule(boxes, "underemployment_rate", data = labour_force, region = region_selected, "Underemployment rate (proportion of labour force)", reverse = T, percent = T)
  callModule(boxes, "underutilisation_rate", data = labour_force, region = region_selected, "Underutilisation rate", reverse = T, percent = T)

  callModule(boxes, "hours_worked_total", data = labour_force, region = region_selected, "Monthly hours worked in all jobs")
  callModule(boxes, "participation_rate", data = labour_force, region = region_selected, "Participation rate", percent = TRUE)
  callModule(boxes, "labour_force_total", data = labour_force, region = region_selected, "Labour force total")
  

  #Industry Employment 
  callModule(boxesAlt, "industry_total", data = employment_industry, region = region_selected,  "Employed total")
  callModule(boxesAlt, "industry_ft", data = employment_industry, region = region_selected, "Employed full-time")
  callModule(boxesAlt, "industry_pt", data = employment_industry, region = region_selected, "Employed part-time")
  callModule(boxesAlt, "industry_under", data = employment_industry, region = region_selected, "Underemployed total")
  

  
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


shinyApp(ui = ui, server = server)
