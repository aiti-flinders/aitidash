dashboardUI <- function(id, data) {
  ns <- NS(id)
  
  lf_next_release <- aitidata::abs_next_release("labour-force-australia")
  industry_next_release <- aitidata::abs_next_release("labour-force-australia-detailed")

  fluidPage(
  h1(textOutput("region_selected")),
  h2("Jobkeeper Insights"),
  p("Based on data released on December 18th for September."),
  fluidRow(
    boxesUI("jobkeeper_total"),
    boxesUI("jobkeeper_proportion")
  ),
  # h2("Jobseeker Insights"),
  # fluidRow(
  #   boxesUI("jobseeker_total"),
  #   boxesUI("ya_total")
  # ),
  h2("Employment Insights"),
  p(paste0("This data is current as of ", reportabs::release(aitidata::labour_force, "month"), " ", reportabs::release(aitidata::labour_force, "year"),".")),
  p(paste0("Data for ",
           reportabs::release(aitidata::labour_force, "month", plus = 1),
           " will be available on ",
           weekdays(lf_next_release), ", the ", scales::ordinal(lubridate::day(lf_next_release)), " of ", lubridate::month(lf_next_release, abbr = F, label = T), ".")),
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
  p(paste0("This data is current as at: ", reportabs::release(aitidata::employment_by_industry, "month"), " ", reportabs::release(aitidata::employment_by_industry, "year"))),
  p(paste0("Data for ",
           reportabs::release(aitidata::employment_by_industry, "month", plus = 3),
           " will be available on ",
           weekdays(industry_next_release), 
           ", the ", 
           scales::ordinal(lubridate::day(industry_next_release))
           ," ", 
           lubridate::month(industry_next_release, abbr = F, label = T), ".")),

  fluidRow(
    boxesAltUI("industry_total"),
    boxesAltUI("industry_ft")
    
  ),
  fluidRow(
    boxesAltUI("industry_pt"),
    boxesAltUI("industry_under")
  )
  )
}

dashboardServer <- function(id, data, region) {
  moduleServer(
    id,
    function(input, output, session) {
      region_selected <- reactive(input$region_select)
      output$region_selected <- renderText({
        region_selected() 
      })
    }
  )
}