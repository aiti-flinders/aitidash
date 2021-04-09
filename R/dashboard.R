dashboardUI <- function(id, data) {
  ns <- NS(id)
  
  lf_release <- list("current" = aitidata::abs_current_release("labour-force-australia"))


  industry_release <- list("nxt" = aitidata::abs_next_release("labour-force-australia-detailed"),
                           "current" = aitidata::abs_current_release("labour-force-australia-detailed"))
  
  
  
  fluidPage(
    h1(textOutput("region_selected")),
    h2("Employment Insights"),
    fluidRow( 
      boxes_ui("jobkeeper_total", plot = FALSE, footer = em("Data last updated: 2021-03-26")),
      boxes_ui("jobkeeper_proportion", plot = FALSE, footer = em("Data last updated: 2021-03-26")),
      purrr::pmap(box_map(), function(id, indicator, footer, ...) boxes_ui(id, indicator, em(paste0(footer, lf_release$current))))
    ),
    h2("Industry Insights"),
    p(paste0("This data is current as at: ", reportabs::release(aitidata::employment_by_industry, "month"), " ", reportabs::release(aitidata::employment_by_industry, "year"))),
    p(paste0("Data for ",
             reportabs::release(aitidata::employment_by_industry, "month", plus = 3),
             " will be available on ",
             weekdays(industry_release$nxt), 
             ", the ", 
             scales::ordinal(lubridate::day(industry_release$nxt))
             ," ", 
             lubridate::month(industry_release$nxt, abbr = F, label = T), ".")),
    
    fluidRow(
      boxes_ui_industry("industry_total"),
      boxes_ui_industry("industry_ft")
      
    ),
    fluidRow(
      boxes_ui_industry("industry_pt"),
      boxes_ui_industry("industry_under")
    )
  )
}


