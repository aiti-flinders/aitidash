#Status Boxes

boxesUI <- function(id) {
  ns <- NS(id)
  
  infoBoxOutput(ns('box'), width = 6)
  
  
  
}

boxes <- function(input, output, session, data, indicator, reverse = F, percent = F) {
  indicator_name <- as.character(indicator)
  unit_name <- as.character(data$unit)[1]
  box_text_current <- current(data, indicator_name, "South Australia", "Trend") 
  box_text_yoy <- last_year(data, indicator_name, "South Australia",  "Trend")['num']
  
  
  if(percent == T) {
    box_text_current <-  str_c(box_text_current %>% formatC(format = 'f', digits = 1), "%")
    box_text_yoy <- str_c(box_text_yoy %>% formatC(format = 'f', digits = 1), " %pt")
    
  } else {
    box_text_current <- str_c(box_text_current %>% formatC(format = 'd', digits = 0, big.mark = ','), unit_name)
    box_text_yoy <- str_c(box_text_yoy %>% formatC(format = 'd', digits = 0, big.mark = ','), unit_name)
  }
  
  if(reverse == T) {
    output$box <- renderInfoBox({
      infoBox(
        indicator_name,
        box_text_current,
        icon = icon(
          ifelse(
            last_year(data, indicator_name, "South Australia", "Persons", "Trend")['num'] < 0,
            'arrow-alt-circle-down',
            'arrow-alt-circle-up'
          )
        ),
        fill = T,
        subtitle = str_c(
          "Year on year: ",
          box_text_yoy),
        color = ifelse(
          last_year(data, indicator_name, "South Australia", "Persons", "Trend")['num'] <0,
          "green",
          "red"),
        width = 4
      )
    })
  } else {
    output$box <- renderInfoBox({
      infoBox(
        indicator_name,
        box_text_current,
        icon = icon(
          ifelse(
            last_year(data, indicator_name, "South Australia", "Trend")['num'] < 0,
            'arrow-alt-circle-down',
            'arrow-alt-circle-up'
          )
        ),
        fill = T,
        subtitle = str_c(
          "Year on year: ",
          box_text_yoy),
        color = ifelse(
          last_year(data, indicator_name, "South Australia", "Trend")['num'] < 0,
          "red",
          "green"
        ),
        width = 4
      )
    })
  }
} 