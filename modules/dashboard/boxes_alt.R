

#Status Boxes

boxesAltUI <- function(id) {
  ns <- NS(id)
  
  infoBoxOutput(ns('box'), width = 4)
  
  
  
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
    ly <-
      last_value(data, list(indicator = indicator, state = region()), print = FALSE) %>%
      pull(value)
    
    cu <-
      current(data, list(indicator = indicator, state = region()), print = FALSE) %>%
      mutate(growth = value - ly,
             growth_pct = (value - ly) / ly) %>%
      filter(value == max(value))
    
    box_text_current <- as_comma(cu$value)
    box_text_yoy <- as_comma(cu$growth)
    
    title <- indicator
    subtitle <- str_c(box_text_current, " (", box_text_yoy, ")")
    ind <- cu$industry
    color <- ifelse(cu$growth > 0, 'green', 'red')
    icon <- icon_tib %>% filter(industry ==  ind) %>% pull(icon)
    
    infoBox(
      fill = T,
      title = title,
      value =  ind,
      subtitle = subtitle,
      color = color,
      icon = icon(icon)
    )
  })
}

