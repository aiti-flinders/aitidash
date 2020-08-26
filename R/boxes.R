#Status Boxes

boxesUI <- function(id) {
  ns <- NS(id)
  
  shinycssloaders::withSpinner(infoBoxOutput(ns('box'), width = 6), 
                                             image = "https://github.com/hamgamb/aitidash/blob/master/www/aiti_spinner.gif?raw=true")
  
  
  
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
    "Monthly hours worked in all jobs", "Hours Worked"
  )
  
  
  output$box <- renderInfoBox({
    
    
    val <- nrow(data[data$indicator == indicator & data$state == region(), ])
    
    if(val == 0) {
      
      box_title <- names_tib[names_tib$indicator == indicator, ]$label
      box_text_current <- "Data Not Available"
      box_subtitle <- paste("For", region())
      colour <- "yellow"
      icon <- "ban"
      
    } else {
      
      box_title <- names_tib[names_tib$indicator == indicator, ]$label
      
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
        box_text_yoy <- paste0(ifelse(ly > 0, "+", ""), as_percentage_point(ly))
        box_text_mom <- paste0(ifelse(lm > 0, "+", ""), as_percentage_point(lm))
        box_text_current <- paste0(as_percent(cu), " (",box_text_mom,")")
        
      } else {
        box_text_yoy <- paste0(ifelse(ly > 0, "+", ""), as_comma(ly))
        box_text_mom <- paste0(ifelse(lm > 0, "+", ""), as_comma(lm))
        box_text_current <- paste0(as_comma(cu), " (",box_text_mom,")")
      }
      
      if(reverse == T) {
        colour <- ifelse(lm < 0,
                         'green', 
                         'red')
        
        icon <- ifelse(lm < 0,
                       'arrow-down', 
                       'arrow-up')
      } else { 
        colour <- ifelse(lm > 0,
                         'green', 
                         'red')
        icon <- ifelse(lm > 0,
                       'arrow-up', 
                       'arrow-down')
      }
      
      box_subtitle <- paste0("Change over year: ", box_text_yoy)
      
    }
    
    box <- infoBox(width = 6, 
                   fill = T, 
            title = box_title,
            value = box_text_current,
            color = colour, 
            icon = icon(icon),
            subtitle = box_subtitle)
    
    return(box)
  })
  
  
}


