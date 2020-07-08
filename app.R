library(tidyverse)
library(lubridate)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(reportabs)
library(aititheme)
#### Preamble ####
# Plotly Setup
Sys.setenv("plotly_username"="hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')


# Font Setup
dir.create("~/.fonts")
file.copy("www/Roboto.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Load Modules
#Dashboard
source('modules/dashboard/boxes.R')
source('modules/dashboard/boxes_alt.R')
#Labour Market
source("modules/labour_market/tab_1_labour_market.R")
source('modules/labour_market/tab_2_labour_market_region_comparison.R')
source('modules/labour_market/tab_3_labour_market_demographic_comparison.R')
source('modules/labour_market/tab_4_labour_market_analysis.R')
#Employment by Industry
source('modules/employment_by_industry/tab_1_employment_by_industry.R')
source('modules/employment_by_industry/tab_2_employment_by_industry_region_comparison.R')
source('modules/employment_by_industry/tab_3_employment_by_industry_analysis.R')
#Internet Vacancies
source("modules/internet_vacancies/tab_1_ivi.R")





#### Header Controls ####
header <- dashboardHeader(#titleWidth = "200px",
                          title = tags$img(src = "statz.png", height = "50px"),
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
      text = "User Guide",
      tabName = "user_guide",
      icon = icon("book-reader")
    ),
    menuItem(
      text = "Dashboard",
      tabName = 'dashboard',
      icon = icon("gear"),
      selected = TRUE
    ),
    menuItem(
      text = "Employment Insights",
      tabName = "labour_market",
      icon = icon("briefcase")
    ),
    menuItem(
      text = "Employment by Industry",
      tabName = "industry",
      icon = icon('industry'),
      badgeLabel = "new", badgeColor = "red"
    ),
    # menuItem(
    #   text = "Internet Vacancies",
    #   tabName = "internet_vacancies",
    #   icon = icon('newspaper')
    # ),
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
  h2("User Guide"),
p("Welcome to the Australian Industrial Transformation Institute's Economic Indicator Dashboard. 
    You can navigate through the different modules by selecting them from the side menu."),
p("Download charts you have created, and the data behind them in the Downloads section"),
p("This dashboard is currently in beta, and may occasionally show errors to the user. The most common cause for an error is the 
    selection of a combination of Region, Indicator, Series Type, and Date, for which data is not available. Selecting another combination
    of variables should solve any problems."),
p("Additional modules are currently in development, and will be added as they are completed. Please check back regularly to 
    have access to the latest data."),
p("For any comments, requests, or issues, please contact", a(href = 'mailto:aiti@flinders.edu.au', "aiti@flinders.edu.au")),
  h3("Definitions"),
p(tags$b("Indicator: "), "A time series variable, measured and collected by the ABS."),
p(tags$b("Series Type: "), "How the observed data has been processed by the ABS."),
p("Original: The observed, unprocessed data."),
p("Seasonally Adjusted: Observed data processed to remove influences that are systematic and calendar related."),
p("Trend: Observed data processed to remove calendar related, and other irregular effects, to show the long term movement of an indicator."),
p(tags$b("Region: "), "States and Territories in Australia, or Australia itself."),
h3("Notes on Data Availability"),
p("Data for this dashboard is sourced from the Australian Bureau of Statistics. There is not universal coverage across regions,
    economic indicators, or series types. Due to the impact of the Coronavirus, there is currently no Trend series available for Australia,
    or the States, and no Seasonally Adjusted series available for the Territories."),
h2("Dashboard"),
p("The boxes on the Dashboard show the current value, monthly change, and yearly change for 12 key labour market indicators.
Those shown in ",
HTML("<b style = 'color:#64b478'>green</b>"), " represent an improvement since last month, and those shown in ",
HTML("<b style = 'color:#ffb24d'>orange</b>"), "represent a deterioration over the previous month.",
"The arrows show in which direction the indicator has moved over the previous month."),
h2("Employment Insights"),
h3("Tab 1"),
p("This tab shows the time series of an indicator,  for a region selected in the sidebar menu."),
h3("Regional Comparison"),
p("This tab allows for the comparison of a given indicator across as many regions as you like. In order to compare States with Territories
  the selected Series Type must be 'Original'"),
h3("Demography"),
p("This tab allows for a breakdown of a given indicator by demographic variables. Age is only available for Australia."),
h2("Employment by Industry")
)

#### Dashboard Tab ####
dashboard_tab <- tabItem(
  tabName = 'dashboard',
  h2("Employment Insights"),
  h2(textOutput("region_selected")),
  uiOutput("release_date"),
  fluidRow(
    boxesUI('employment_total'),
    boxesUI('employment_ft'),
    boxesUI('employment_pt')
  ),
  fluidRow(
    boxesUI('unemployed'),
    boxesUI('underemployed'),
    boxesUI('underutilised')
  ),
  fluidRow(
    boxesUI('unemployment_rate'),
    boxesUI('underemployment_rate'),
    boxesUI('underutilisation_rate')
  ),
  fluidRow(
    boxesUI('hours_worked_total'),
    boxesUI('participation_rate'),
    boxesUI('labour_force_total')
  ),
  h2("Industry Insights"),
  HTML(
    str_c("The boxes below show which industry employs the most people overall, full-time, and part-time. Each box displays the industry name, the number of people employed, and the monthly change. ")
  ),
  uiOutput("release_date_industry"),
  fluidRow(
    boxesAltUI("industry_total"),
    boxesAltUI("industry_ft"),
    boxesAltUI("industry_pt")
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
           labourMarketDemogUI("lm_demog", data = labour_force),
           labourMarketAnalysisUI("lm_analysis", data = labour_force)
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
      empIndComparisonUI("empInd_region", data = employment_industry),
      empIndAnalysisUI("empInd_analysis", data = employment_industry)
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
      iviUI("ivi_ts", data = internet_vacancies_basic)
    )
  )
)
                         
#### Body ####                                       
body <- dashboardBody(tags$head(tags$link(rel = "stylesheet", type = 'text/css', href = 'custom.css')),
  tabItems(
    user_guide_tab,
    dashboard_tab,
    labour_market_tab,
    emp_ind_tab
    #internet_vacancies_tab
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
  
  output$release_date <- renderUI(str_c("This data is current as at: ", release(labour_force, "month"), " ", release(labour_force, "year")))
  output$release_date_industry <- renderUI(str_c("This data is current as at: ", release(employment_industry, "month"), " ", release(employment_industry, "year")))
  
  region_selected <- reactive(input$region_select)
  output$region_selected <- renderText({
    region_selected()
  })

  #Labour Market -  Tab
  callModule(labourMarket, "lm_ts", data = labour_force, region = region_selected)
  callModule(labourMarketRegional, "lm_region", data = labour_force, region = region_selected)
  callModule(labourMarketDemog, "lm_demog", data = labour_force, region = region_selected)
  callModule(labourMarketAnalysis, "lm_analysis", data = labour_force, region = region_selected)

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = employment_industry, region = region_selected)
  callModule(empIndComparison, "empInd_region", data = employment_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = employment_industry, region = region_selected)
  
  #IVI - Tab
  #callModule(ivi, "ivi_ts", data = internet_vacancies_basic, region = region_selected)
  
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
