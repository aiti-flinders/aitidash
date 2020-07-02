#Status Boxes

boxesUI <- function(id) {
  ns <- NS(id)
  
  infoBoxOutput(ns('box'), width = 4)
  
  
  
}

boxes <- function(input, output, session, data, indicator, region, reverse = F, percent = F) {
  
  names_tib <- tribble(
    ~indicator, ~label, 
    "Employed total", "Employed Total",
    "Employed full-time", "Employed Full Time",
    "Employed part-time", "Employed Part Time",
    "Unemployment rate", "Unemployment Rate",
    "Unemployed total", "Unemployed Total",
    "Labour force total", "Labour Force Total",
    "Underemployed total", "Underemployed Total",
    "Underutilised total", "Underutilised Total",
    "Participation rate", "Participation Rate",
    "Employment to population ratio", "Employment to Population Ratio",
    "Underemployment rate (proportion of labour force)", "Underemployment Rate",
    "Underutilisation rate", "Underusilisation Rate",
    "Monthly hours worked in all jobs", "Hours Worked",
  )
  

  output$box <- renderInfoBox({
    
    cu <- current(data, list('indicator' = indicator, 
                             'state' = region()),
                  print = FALSE) 
    ly <- cu - last_value(data, list('indicator' = indicator,
                               'state' = region()), 
                   "year",
                   print = FALSE)
    lm <- cu - last_value(data, list('indicator' = indicator,
                                     'state' = region()),
                                     'month',
                                     print = FALSE)
    
    if(percent == T) {
      box_text_current <- as_percent(cu)
      box_text_yoy <- as_percent(ly)
      box_text_mom <- as_percent(lm)
    } else {
      box_text_current <- as_comma(cu)
      box_text_yoy <- as_comma(ly)    
      box_text_mom <- as_comma(lm)
    }
    
    if(reverse == T) {
      colour <- ifelse(lm < 0,
                       'green', 
                       'red')
      
      icon <- ifelse(lm < 0,
                     'arrow-alt-circle-down', 
                     'arrow-alt-circle-up')
    } else { 
      colour <- ifelse(lm > 0,
                     'green', 
                     'red')
      icon <- ifelse(lm > 0,
                   'arrow-alt-circle-up', 
                   'arrow-alt-circle-down') }
    ind <- data$indicator
    

    infoBox(fill = T, 
            title = names_tib[names_tib$indicator == indicator, ]$label,
            value = box_text_current,
            color = colour, 
            icon = icon(icon),
            subtitle = str_c("Month: ", box_text_mom,  " Year: ", box_text_yoy))
   
  })

  
  }
  
  
