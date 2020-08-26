covidDemographicUI <- function(id, data) {
  ns <- NS(id)
  
  tabPanel("Demographics")
  
}

covidDemographicServer <- function(id, data, region) {
  
  moduleServer(
    id,
    function(input, output, session) {
      

    }
  )
}