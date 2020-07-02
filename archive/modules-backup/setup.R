setupInput <- function(id) {
  ns <- NS(id)
}

setup <- function(input, output, session){
  input_region <- reactive({input$state})
  return(input_region)
}