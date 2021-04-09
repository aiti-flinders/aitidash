sidebar <- function(id) {
  ns <- NS(id)
  
  sidebarMenu(
    id = "sidebarmenu",
    menuItem(
      text = "Summary",
      tabName = 'dashboard',
      icon = icon("chart-bar"),
      selected = TRUE
    ),
    # menuItem(
    #   text = "User Guide",
    #   tabName = "user_guide",
    #   icon = icon("book-reader")
    # ),
    menuItem(
      text = "Employment Insights",
      tabName = "employment_insights",
      icon = icon("briefcase"),
      menuSubItem(
        text = "Labour Force",
        tabName = "employment",
        icon = icon("briefcase")
      ),
      menuSubItem(
        text = "Weekly Payroll",
        tabName = "jobs_payroll",
        icon = icon("chart-line")
      ),
      menuSubItem(
        text = "Small Areas",
        tabName = "salm",
        icon = icon("map")
      )
    ),
    menuItem(
      text = "Industry Insights",
      tabName = "industry_insights",
      icon = icon('industry'),
      menuSubItem(
        text = "Employment by Industry",
        tabName = "industry_employment",
        icon = icon("industry")
      ),
      menuSubItem(
        text = "Weekly Payroll",
        tabName = "industry_payroll",
        icon = icon("chart-line")
      )
    ),
    menuItem(
      text = "Maps",
      tabName = "maps",
      icon = icon("map")
    )
  )
}