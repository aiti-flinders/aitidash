user_guide <- function(id) {
  ns <- NS(id)
  
tabPanel(title = "User Guide",
         fluidPage(
           h2("Welcome"),
  p("Welcome to the Australian Industrial Transformation Institute's Economic Indicator Dashboard. 
    You can navigate through the different modules by selecting them from the side menu."),
  p("This dashboard is currently in beta, and may occasionally show errors to the user."),
  p("The most common cause for an error is the selection of a combination of", em('Region,', 'Indicator,', 'Series Type'), "and", em('Date'), "for which data is not available. Selecting another combination
    of variables should solve any problems."),
  p("Additional modules are currently in development, and will be added as they are completed. The Summary, and Employment Insights modules 
are updated on approximately the third thursday of the month when the ABS Labour Force Survey is released. The Industry Insights module is updated 
every quarter when the ABS Detailed Labour Force Survey is released. Check back regularly for access to the latest data."),
  p("Any chart, as it appears on screen, as well as the data behind them, can be downloaded in the Downloads section of each module."),
  p("For any comments, requests, or issues, please contact", a(href = 'mailto:aiti@flinders.edu.au', "aiti@flinders.edu.au"),"."),
  h2("Definitions"),
  p(tags$b("Indicator: "), "A time series variable, measured and collected by the ABS."),
  p(tags$b("Series Type: "), "How the observed data has been processed by the ABS."),
  p("Original: The observed, unprocessed data."),
  p("Seasonally Adjusted: Observed data processed to remove influences that are systematic and calendar related."),
  p("Trend: Observed data processed to remove calendar related, and other irregular effects, to show the long term movement of an indicator. 
    Note that the Trend Series is currently suspended for many Indicators."),
  p(tags$b("Region: "), "States and Territories in Australia, or Australia itself."),
  h2("Notes on Data Availability"),
  p("Data for this dashboard is sourced from the Australian Bureau of Statistics. There is not universal coverage across regions,
    economic indicators, or series types. Due to the impact of the Coronavirus, there is currently no Seasonally Adjusted Series available for the Territories.")))
}