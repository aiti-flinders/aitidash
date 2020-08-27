sidebarUI <- function(id) {
  ns <- NS(id)
  
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
      menuItem(
        text = "Labour Force",
        tabName = "labour_market",
        icon = icon("chart-line")
      ),
      menuItem(
        text = "Internet Vacancies",
        tabName = "internet_vacancies",
        icon = icon("newspaper")
      )
    ),
    menuItem(
      text = "Industry Insights",
      tabName = "industry_insights",
      icon = icon('industry'),
      menuItem(
        text = "Industry Employment",
        tabName = "industry",
        icon = icon("city")
      )
    ),
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
      menuItem(
        text = "Maps",
        tabName = "covid_map",
        icon = icon("map"),
        badgeLabel = "new!"),
      menuItem(
        text = "Payroll Jobs",
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
} 