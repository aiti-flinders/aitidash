specificInstructionsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Module Specific Information",
           fluidPage(
             h2("Summary"),
             p("The boxes on the Dashboard show the current value, monthly change, and yearly change for 12 key labour market indicators.
Those shown in ",
               HTML("<b style = 'color:#64b478'>green</b>"), " represent an improvement since last month, and those shown in ",
               HTML("<b style = 'color:#ffb24d'>orange</b>"), "represent a deterioration over the previous month.",
               "The arrows show in which direction the indicator has moved over the previous month. Note that for indicators such as the Unemployment Rate, a
               decrease (downward arrow) is considered an improvement (shown in green)."),
             h2("Employment Insights"),
             h3("Tab 1"),
             p("This tab shows the time series of an indicator for the region selected in the sidebar menu."),
             h3("Regional Comparison"),
             p("This tab allows for the comparison of a given indicator across as many regions as you like. In order to compare States with Territories
  the selected Series Type must be 'Original'"),
             h3("Demography"),
             p("This tab allows for a breakdown of a given indicator by demographic variables. Age is only available for Australia."),
             h3("Small Area Unemployment Rates"),
             p("This tab shows smoothed unemployment rates for all SA2 Statistical Areas in Australia, or only those in a region if a region
               is selected using data from the Small Area Labour Markets publication produced by the Department of Employment. The download button
               on this page will download the map as it is visible to you. However please not that there may be some delays if attempting to download
               a map with all SA2 Statistical Areas shown."),
             h2("Industry Insights"),
             h3("Tab 1"),
             p("This tab shows the breakdown of employment by industry for the region selected in the sidebar menu, for a given year, and month. 
    Selecting an industry shows the historic time series data for the selected indicator in a specific industry. Up to 9 industries can be compared."),
             h3("Regional Comparison"),
             p("This tab shows the differences between the share of employment in an industry between the region selected in the sidebar menu, and the comparison
    region selected for a given year."),
             h2("COVID-19: Maps"),
             p("This tab shows how the COVID-19 global pandemic has affected small regions within Australia. It shows at the Statistical Area 2 level: "),
             tags$ul(
               tags$li("Approximate number of businesses receiving JobKeeper"),
               tags$li("Number of individuals receiving JobSeeker"),
               tags$li("Approximate proportion of businesses receiving JobKeeper"),
               tags$li("Proportion of individuals receiving JobSeeker")
             ),
             p("Note that the JobKeeper analysis is approximate as only PostCode data has been made available which does not directly convert to a Statistical Area. 
               In addition, the latest business count data is for June 2019."),
             p("At the SA4 Level, it shows the change in the number of payroll jobs - indexed to the week ending the 14th of March 2020"),
             h2("COVID-19: Payroll Jobs"),
             h3("Regional Comparison, Demography"),
             p("These tabs use data from the Weekly Payroll Jobs in Australia dataset published by the ABS. It allows comparison across States
             and Territories, with additional drilldowns based on gender and age"),
             h3("Industry"),
             p("This tab shows the change in employment by industry for a selected region, indexed to March 14th - the week when Australia recorded
             its 100th COVID-19 case. As such, the bar chart will show nothing until the weeks slider is adjusted. Selecting an industry shows
             the change in Payroll jobs since the week ending January 6th 2020."),
             p("Note that the payroll jobs index is measured differently to the number of people employed. For more information see: ",
               a(href = "https://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6160.0.55.001Explanatory%20Notes1Week%20ending%2025%20July%202020?OpenDocument","Weekly Payroll Jobs and Wages in Australia"))
           )
  )
}