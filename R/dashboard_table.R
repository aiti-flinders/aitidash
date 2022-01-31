table_ui <- function(id) {
  
  
  ns <- NS(id)
  
  
  htmlOutput(ns("table"))
  
  
}

table_server <- function(id, data, region) {
  moduleServer(
    id,
    function(input, output, session) {
      
      all_indicators <- c("Unemployment rate",
                          "Unemployed total",
                          "Employed total",
                          "Participation rate",
                          "Employed full-time",
                          "Employed part-time",
                          "Underemployed total",
                          "Underutilised total",
                          "Underemployment rate (proportion of labour force)",
                          "Underutilisation rate",
                          "Monthly hours worked in all jobs",
                          "Labour force total")
      
      
      output$table <- renderUI({
      
      using <- data %>%
        filter(gender == "Persons",
               age == "Total (age)",
               state == region(),
               indicator %in% all_indicators,
               series_type == "Seasonally Adjusted") %>%
        distinct(state, indicator, series_type) %>%
        as.list()
      
      sparklines <- data %>%
        filter(gender == "Persons", 
               age == "Total (age)",
               state == region(),
               indicator %in% all_indicators,
               series_type == "Seasonally Adjusted",
               year >= 2019) %>%
        group_by(indicator) %>%
        mutate(sparkline = spk_chr(value, type = "line", width = "160px", height = "50px")) %>%
        ungroup() %>%
        distinct(indicator, sparkline)
      
      current <- data %>%
        filter(indicator %in% using$indicator,
               state == region(),
               series_type == "Seasonally Adjusted",
               age == "Total (age)",
               gender == "Persons",
               date == max(.$date)) %>%
        select(indicator, unit, current = value)
      
      over_month <- data %>%
        value_at(data = ., filter_with = using, at_month = "November") %>%
        select(indicator, last_month = value)
      
      over_year <- data %>%
        value_at(data = ., filter_with = using, at_year = 2020) %>% 
        select(indicator, last_year = value)
      
      table_data <- left_join(current, over_month, by = "indicator") %>%
        left_join(over_year, by = "indicator") %>%
        mutate(change_over_month = current - last_month,
               change_over_year = current - last_year,
               reverse = case_when(
                 indicator %in% c("Unemployed total",
                                  "Underemployed total",
                                  "Underutilised total",
                                  "Unemployment rate",
                                  "Underemployment rate (proportion of labour force)",
                                  "Underutilisation rate") ~ TRUE,
                 TRUE ~ FALSE
               ),
               up_month = case_when(
                 change_over_month > 0 & reverse ~ "red",
                 change_over_month > 0 & !reverse ~ "green",
                 change_over_month < 0 & reverse ~ "green",
                 TRUE ~ "red"
               ), 
               up_year = case_when(
                 change_over_year > 0 & reverse ~ "red",
                 change_over_year > 0 & !reverse ~ "green",
                 change_over_year < 0 & reverse ~ "green",
                 TRUE ~ "red")) %>%
        left_join(sparklines, by = "indicator") %>%
        mutate(current = case_when(
          unit == "000" ~ as_comma_group(., group = "indicator", value = "current"),
          unit == "Percent" ~ as_percent(current)),
          change_over_month = case_when(
            unit == "000" ~ as_comma_group(., group = "indicator", value = "change_over_month"),
            unit == "Percent" ~ as_percent(change_over_month)),
          change_over_year = case_when(
            unit == "000" ~ as_comma_group(., group = "indicator", value = "change_over_year"),
            unit == "Percent" ~ as_percent(change_over_year))) %>%
        rename(Indicator = indicator,
               "Current value" = current,
               "Monthly change" = change_over_month,
               "Yearly change" = change_over_year,
               "Trend (2 years)" = sparkline)
      
      
      out <- formattable(
        table_data,
        list(unit = F,
             last_month = F,
             last_year = F,
             reverse = F,
             up_month = F,
             up_year = F,
             `Current value` = formatter("span", style = style(font.weight = "bold")),
             `Monthly change` = formatter("span", style = ~style(color = ifelse(up_month == "red", "#ffb24d", "#64b478"))),
             `Yearly change` = formatter("span", style = ~style(color = ifelse(up_year == "red", "#ffb24d", "#64b478")))
        )) %>%
        format_table() %>%
        HTML() %>%
        div() %>% 
        spk_add_deps()
      
      out
      
      })
      
      
      
    }
  )
}
