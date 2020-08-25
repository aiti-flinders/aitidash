

#Status Boxes

boxesAltUI <- function(id) {
  ns <- NS(id)
  
  infoBoxOutput(ns('box'), width = 6)
  
  
  
}

boxesAlt <- function(input, output, session, data, indicator, region, growth = F, percent = F) {
  
  data <- data %>% filter(industry != "Total (industry)")
  
  icon_tib <- tribble(
    ~icon, ~industry,
    'tractor', "Agriculture, Forestry and Fishing",
    "mountain", "Mining",
    "hard-hat", "Manufacturing",
    "plug", "Electricity, Gas, Water and Waste Services",
    "hammer", "Construction",
    "warehouse", "Wholesale Trade",
    "shopping-basket", "Retail Trade",
    "utensils", "Accommodation and Food Services",
    "mail-bulk", "Transport, Postal and Warehousing",
    "network-wired", "Information Media and Telecommunications",
    "coins", "Financial and Insurance Services",
    "home", "Rental, Hiring and Real Estate Services",
    "microscope", "Professional, Scientific and Technical Services",
    "landmark", "Administrative and Support Services",
    "user-shield", "Public Administration and Safety",
    "school", "Education and Training",
    "hospital", "Health Care and Social Assistance",
    "palette", "Arts and Recreation Services",
    "user", "Other Services")
  
  output$box <- renderInfoBox({
    
    val <- nrow(data[data$indicator == indicator & data$state == region(), ])
    
    if (val == 0) {
      box_title <- indicator
      box_value <- "Data Not Available"
      box_subtitle <- paste("For", region())
      colour <- "yellow"
      icon <- "ban"
    } else {
      
      box_title <- indicator
      
      ly <- last_value(data, list(indicator = indicator, state = region()), print = FALSE) %>%
        pull(value)
      
      cu <- current(data, list(indicator = indicator, state = region()), print = FALSE) %>%
        mutate(growth = value - ly,
               growth_pct = (value - ly) / ly) %>%
        filter(value == max(value))
      
      box_text_current <- as_comma(cu$value)
      box_text_yoy <- paste0(ifelse(cu$growth > 0, "+", ""), as_comma(cu$growth))
      
      box_subtitle <- paste0(box_text_current, " (", box_text_yoy, ")")
      
      box_value <- cu$industry
      
      colour <- case_when(
        cu$growth >0 & indicator != "Underemployed total" ~ "green",
        TRUE ~'red'
        )
      
      icon <- icon_tib %>% 
        filter(industry ==  box_value) %>% 
        pull(icon)
      
    }
      
      box <- infoBox(width = 6,
                     fill = T,
                     title = box_title,
                     value =  box_value,
                     subtitle = box_subtitle,
                     color = colour,
                     icon = icon(icon))
      
      return(box)

  })
}

