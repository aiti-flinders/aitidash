userGuideUI <- function(id) {
  ns <- NS(id)
  
fluidPage(
  h2("User Guide"),
  p("Welcome to the Australian Industrial Transformation Institute's Economic Indicator Dashboard. 
    You can navigate through the different modules by selecting them from the side menu."),
  p("This dashboard is currently in beta, and may occasionally show errors to the user. The most common cause for an error is the 
    selection of a combination of Region, Indicator, Series Type, and Date, for which data is not available. Selecting another combination
    of variables should solve any problems."),
  p("Additional modules are currently in development, and will be added as they are completed. The Dashboard, and Employment Insights modules 
are updated on approximately the third thursday of the month when the ABS Labour Force Survey is released. The Industry Insights module is updated 
every quarter when the detailed ABS Labour Force Survey is released. Check back regularly for access to the latest data."),
  p("Any charts you have created, and the data behind them, can be downloaded in the Downloads section of each module"),
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
  p("This tab shows the time series of an indicator for the region selected in the sidebar menu."),
  h3("Regional Comparison"),
  p("This tab allows for the comparison of a given indicator across as many regions as you like. In order to compare States with Territories
  the selected Series Type must be 'Original'"),
  h3("Demography"),
  p("This tab allows for a breakdown of a given indicator by demographic variables. Age is only available for Australia."),
  h2("Industry Insights"),
  h3("Tab 1"),
  p("This tab shows the breakdown of employment by industry for the region selected in the sidebar menu, for a given year, and month. 
    Selecting an industry shows the historic time series data for the selected indicator in a specific industry. Up to 9 industries can be compared."),
  h3("Regional Comparison"),
  p("This tab shows the differences between the share of employment in an industry between the region selected in the sidebar menu, and the comparison
    region selected for a given year.")
  )
  
}