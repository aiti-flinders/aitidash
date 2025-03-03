## code to prepare `dashboard_data` dataset goes here
library(reportabs)
library(readabs)
library(tidyverse)

labour_force <- read_absdata("labour_force") |> 
  select(date, indicator, sex, age, state, value, series_type, unit) |> 
  mutate(month = month(date, label = TRUE, abbr = FALSE),
         year = year(date),
         value = ifelse(unit == "000", 1000*value, value))

usethis::use_data(labour_force, compress = 'xz', overwrite = TRUE)

industry_employment <- read_absdata("industry_employment") |> 
  select(date, indicator, industry, state, series_type, value, value_share, unit)

usethis::use_data(industry_employment, compress = 'xz', overwrite = TRUE)

dashboard_data <- bind_rows(
  labour_force,
  read_absdata("hours_worked")
) 

relevant_dates <- c(max(dashboard_data$date),
                    max(dashboard_data$date - years(1)))

dashboard_data <- dashboard_data |> 
  filter(between(date, min(relevant_dates), max(relevant_dates))) |> 
  mutate(year = year(date))

usethis::use_data(dashboard_data, compress = 'xz', overwrite = TRUE)  
         
