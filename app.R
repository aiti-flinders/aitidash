library(tidyverse)
library(lubridate)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(reportabs)
library(aititheme)
theme_set(theme_bw()+ 
            theme(
              legend.position = 'none',
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour='black'),
              legend.spacing.x = unit(0.3, 'cm'),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
            ))


#Plotly Setup
Sys.setenv("plotly_username"="hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')

#### Preamble ####

dir.create("~/.fonts")
file.copy("www/Roboto.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

source('modules/dashboard/boxes.R')
source('modules/dashboard/boxes_alt.R')
# source('modules/setup.R')


#Retail Trade
# source("modules/retail_trade_south_australia.R")
# source("modules/retail_trade_region.R")
# source("modules/retail_trade_analysis.R")
# #Labour Market
source("modules/labour_market/tab_1_labour_market.R")
source('modules/labour_market/tab_2_labour_market_region_comparison.R')
source('modules/labour_market/tab_3_labour_market_demographic_comparison.R')
source('modules/labour_market/tab_4_labour_market_analysis.R')
#Exports
#source('modules/exports_south_australia.R')
#Employment by Industry
source('modules/employment_by_industry/tab_1_employment_by_industry.R')
source('modules/employment_by_industry/tab_2_employment_by_industry_region_comparison.R')
source('modules/employment_by_industry/tab_3_employment_by_industry_analysis.R')
#Population
# source('modules/population_south_australia.R')
# source('modules/population_region.R')
# source('modules/population_demographic.R')
# #Output
# source('modules/industry_south_australia.R')
# source('modules/industry_australia.R')
# source('modules/industry_analysis.R')




#rt <- read_retail_trade()
#pop <- read_population()
#pop_pyramid <- read_population_pyramid()
#exports_data <- read_exports_data() 
# state_accs <- read_state_accounts() 
# nat_accs <- read_national_accounts()



#### Header Controls ####
header <- dashboardHeader(#titleWidth = "200px",
                          title = "WorkSight",
                          tags$li(a(href = "http://www.flinders.edu.au/aiti",
                                    img(src = "aiti_logo.png",
                                        title = "Australian Industrial Transformation Institute", height = "17px")),
                                  
                                    class = "dropdown"))

##### Sidebar Controls #####
sidebar = dashboardSidebar(collapsed = FALSE,
  #tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
  width = '230px',
  sidebarMenu(id = 'tabs',
    menuItem(
      text = "Dashboard",
      tabName = 'dashboard',
      icon = icon("gear")
    ),
    menuItem(
      text = "Labour Market",
      tabName = "labour_market",
      icon = icon("briefcase")
    ),
    menuItem(
      text = "Employment by Industry",
      tabName = "industry",
      icon = icon('industry')
    ),
    radioButtons(
      inputId = "region_select",
      label = "Select Region",
      choices = c("Australia","New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory"),
      selected = "Australia"
    )
  )
)



#South Australia Plots
#retail_trade_time_series = retailTradeUI("rt_ts", data = rt)
emp_ind_time_series = empIndUI("empInd_ts", data = employment_industry)
#pop_time_series = populationUI("pop_ts", data = pop)
#exports_time_series = exportsUI("exports_ts", data = exports_data)
#industry_time_series = industryUI('industry_ts', data = state_accs)

#Regional Plots
#retail_trade_regional = retailTradeRegionalUI("rt_region", data = rt)
emp_ind_regional = empIndComparisonUI("empInd_region", data = employment_industry)
#pop_region = populationRegionUI("pop_region", data = pop)
#industry_australia = industryAustraliaUI("industry_australia", data = nat_accs)

#Demographic Plots
#pop_demo = populationDemoUI("pop_demo", data = pop_pyramid)

#Analysis 
#retail_trade_analysis = retailTradeAnalysisUI("rt_analysis", data = rt)
emp_ind_analysis = empIndAnalysisUI("empInd_analysis", data = employment_industry)
#industry_analysis = industryAnalysisUI("industry_analysis")


#### Dashboard Tab ####
dashboard_tab <- tabItem(
  tabName = 'dashboard',
  h2("Employment Insights"),
  HTML(
    str_c("The boxes below show the current value, monthly change, and yearly change for 12 key labour market indicators.
       Those shown in <span class = 'sq blue'></span> represent an improvement, 
       and those shown in <span class = 'sq orange'></span> represent a deterioration. The arrows show in which direction the indicator has moved over the previous month.")
    ),
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
labour_market_tab = tabItem(tabName = 'labour_market',
                            fluidRow(
                              tabBox(id = 'labour_market_tab_id',
                                width = 12,
                                labourMarketUI("lm_ts", data = labour_force),
                                labourMarketRegionalUI("lm_region", data = labour_force),
                                labourMarketDemogUI("lm_demog", data = labour_force),
                                labourMarketAnalysisUI("lm_analysis", data = labour_force)
                              )
                            ))

#Consumer Activity Tab
# retail_trade_tab = tabItem(tabName = 'retail_trade',
#                            fluidRow(
#                              boxesUI("turnover"),
#                              boxesUI("turnover_temp")
#                            ),
#                            fluidRow(
#                              tabBox(width = 12,
#                                     retail_trade_time_series,
#                                     retail_trade_regional,
#                                     retail_trade_analysis)
#                            ))

#Industry Tab

emp_ind_tab = tabItem(tabName = 'industry', fluidRow(
  tabBox(id = "employment_industry_tab_id",
    width = 12,
    emp_ind_time_series,
    emp_ind_regional,
    emp_ind_analysis
  )
))

                         
                                       
body <- dashboardBody(
  tags$head(
     tags$link(rel = "stylesheet", type = 'text/css', href = 'custom.css')
   ),

  tabItems(
    dashboard_tab,
    labour_market_tab,
    emp_ind_tab
  )
)
  
#### UI ####
ui <- dashboardPage(title = "AITI Dashboard",
  header,
  sidebar,
  body
  
)

#### Server ####
server <- function(input, output) {
  
  output$release_date <- renderUI(str_c("This data is current as at: ", release(labour_force, "month"), " ", release(labour_force, "year")))
  output$release_date_industry <- renderUI(str_c("This data is current as at: ", release(employment_industry, "month"), " ", release(employment_industry, "year")))
  
  region_selected <- reactive(input$region_select)

  #Labour Market -  Tab
  callModule(labourMarket, "lm_ts", data = labour_force, region = region_selected)
  callModule(labourMarketRegional, "lm_region", data = labour_force, region = region_selected)
  callModule(labourMarketDemog, "lm_demog", data = labour_force, region = region_selected)
  callModule(labourMarketAnalysis, "lm_analysis", data = labour_force, region = region_selected)

  #Employment by Industry - Tab
  callModule(empInd, "empInd_ts", data = employment_industry, region = region_selected)
  callModule(empIndComparison, "empInd_region", data = employment_industry, region = region_selected)
  callModule(empIndAnalysis, "empInd_analysis", data = employment_industry, region = region_selected)
  
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
