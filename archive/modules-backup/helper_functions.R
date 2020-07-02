library(readabs)
library(strayr)
library(readxl)
library(tidyverse)
library(lubridate)

#### Formatting ####
plot_adjust <- function(scale = 8) {
theme_set(theme_bw() +
            theme(
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.spacing.x = unit(0.3, 'cm'),
              legend.text = element_text(size = scale*2.834646),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.spacing.x = unit(0.3, "cm"),
              plot.caption = element_text(hjust = 0, size = scale*2.834646),
              axis.text = element_text(size = scale*2.834646),
              axis.title = element_text(size = scale*2.834646),
              plot.title = element_text(size = scale*2.834646),
              plot.subtitle = element_text(size = scale*2.834646), 
              strip.text = element_text(size = scale*2.834646)
            ))
}

#### Reader and wranglers ####
#Labour Market Information Portal - Vacancy Data
read_vacancy_report <- function(type = "all") {
  if(type == 'skill') {
    read_excel('../data/lmip_vacancy_skill.xlsx', sheet = "Trend", .name_repair = 'universal') %>%
      gather(key = 'date', value = 'vacancies', c(5:length(.))) %>%
      mutate(date = gsub("[...]", "", date),
             date = as.Date(as.numeric(date), origin = '1899-12-30'),
             region = strayr(State, to = 'state_name', fuzzy_match = F),
             region = ifelse(is.na(region), "Australia", region)) %>%
      group_by(region, date, Skill_level) %>%
      summarise(vacancies = mean(vacancies))%>%
      filter(Skill_level != 0) %>%
      group_by(region, date) %>%
      mutate(share = vacancies/sum(vacancies)) %>%
      ungroup()
    
  } else if(type == 'occ') {
    read_excel('../data/lmip_vacancy_occupation.xlsx', sheet = 2, .name_repair = 'universal') %>%
      gather(key = 'date', value = 'vacancies', c(4:length(.))) %>%
               mutate(date = gsub("[...]", "", date),
                      date = as.Date(as.numeric(date), origin = '1899-12-30'),
                      state = strayr(state, to = 'state_name', fuzzy_match = F),
                      state = ifelse(is.na(state), "Australia", state),
                      vacancies = as.numeric(vacancies)) %>%
               group_by(state, date, ANZSCO_TITLE, ANZSCO_CODE) %>%
               summarise(vacancies = mean(vacancies, na.rm=T)) %>%
               rename(occ = ANZSCO_TITLE,
                      anzsco_4 = ANZSCO_CODE, 
                      region = state) %>%
      mutate(anzsco_1 = str_sub(anzsco_4, start = 1, end = 1),
             anzsco_2 = str_sub(anzsco_4, start = 1, end = 2),
             anzsco_3 = str_sub(anzsco_4, start = 1, end = 3)) %>%
      mutate_at(.vars = c('anzsco_1', 'anzsco_2', 'anzsco_3'), 
                .funs = function(x) str_pad(x, width = 4, side = 'right',pad = '0')) %>%
      ungroup()
  } else if(type == 'basic') {
    read_excel('../data/lmip_vacancy.xlsx', sheet = 1, .name_repair = 'universal') %>%
      gather(key = 'date', value = "vacancies", c(5:length(.))) %>%
      mutate(date = gsub("[...]","", date),
             date = as.Date(as.numeric(date), origin = "1899-12-30"),
             State = strayr(State, to = 'state_name', fuzzy_match = F),
             State = ifelse(is.na(State), "Australia", State)) %>%
      group_by(State, date, ANZSCO_CODE, Title) %>%
      summarise(vacancies=mean(vacancies)) %>%
      rename(region = State,
             occ = Title,
             anzsco_2 = ANZSCO_CODE) %>%
      ungroup()
  } else if (type == 'all') {
    bind_rows(read_vacancy_report(type = 'basic'),
              read_vacancy_report(type = 'skill'),
              read_vacancy_report(type = 'occ'))
  }
  
}

#Participation, Job Search and Mobility, Australia
#WIP

#Labour Force Survey 6202.0
#Table 12 - General Employment/Unemployment
read_labour_force <- function(force = F) {
f_path <- "../data/ABS/labour_force.csv"

release <- data.frame(release_date = as.Date(c("2019-05-16", "2019-06-13", "2019-07-18", "2019-08-15", "2019-09-19", "2019-10-17"))) %>%
  mutate(released = ifelse(release_date <= today(), 1, 0))

next_release <- release %>% 
  filter(released == 0) %>%
  pull(release_date) %>%
  min() 

new_release <- next_release <= today()

f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))

f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release


if(!f_exists | (!f_latest & new_release)) {

lf <- read_abs(cat = "6202.0", tables = c(12), retain_files = F) %>%
  separate_series(column_names = c("indicator", "gender", "state")) %>%
  mutate(value = ifelse(unit == "000", (1000*value), (value)),
         year = year(date),
         month = month(date, label = T, abbr = F),
         age = "Total (age)") %>%
  select(date, month, year, indicator, gender, state, age, value, series_type, unit) %>%
  mutate(id = str_c(date, month, year, indicator, gender, state, age, series_type))

write_csv(lf, "../data/ABS/labour_force.csv")

return(lf)

} else { lf <- read_csv("../data/ABS/labour_force.csv") }
}

#Table 22 and 23 - General underutilisation
read_underutilisation <- function(force = F) {
  f_path <- "../data/ABS/underutilisation.csv"
  
  release <- data.frame(release_date = as.Date(c("2019-05-16", "2019-06-13", "2019-07-18", "2019-08-15", "2019-09-19", "2019-10-17"))) %>%
    mutate(released = ifelse(release_date <= today(), 1, 0))
  
  next_release <- release %>% 
    filter(released == 0) %>%
    pull(release_date) %>%
    min() 
  
  new_release <- next_release <= today()
  
  f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
  
  f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
  
  
  if(!f_exists | (!f_latest & new_release)) {
    
    uu_22 <- read_abs(cat_no = '6202.0', tables = c(22), retain_files = F) %>%
      separate(series, into = c("indicator", "gender", "age"), sep = ';') %>%
      mutate(age = ifelse(age == "", "Total (age)", age),
             value = ifelse(unit == "000", (1000*value), (value)),
             year = year(date),
             month = month(date, label = T, abbr = F),
             state = "Australia") %>%
      mutate_at(.vars = c('indicator', 'gender', 'age'), trimws) %>%
      mutate_at(.vars = c('indicator','gender'), ~str_remove_all(., "> ")) %>%
      distinct() %>%
      select(date, month, year, indicator, gender, state, age, value, series_type, unit) 
    
    uu_23 <- read_abs(cat_no = '6202.0', tables = 23, retain_files = F) %>%
      separate(series, into = c('indicator', 'gender', 'state'), sep = ';') %>%
      mutate(state = ifelse(state == "", "Australia", state),
             value = ifelse(unit == "000", (1000*value), (value)),
             year = year(date),
             month = month(date, label = T, abbr = F),
             age = "Total (age)") %>%
      mutate_at(.vars = c("indicator", "gender", "state"), trimws) %>%
      mutate_at(.vars = c('indicator', 'gender', 'state'), ~str_remove_all(., "> ")) %>%
      distinct() %>%
      select(date, month, year, indicator, gender, state, age, value, series_type, unit)
    
    uu <- bind_rows(uu_22,uu_23) %>%
      mutate(id = str_c(date, month, year, indicator, gender, state, age, series_type))
    
    # mutate(value = ifelse(unit == "000", value*1000, value),
    #        year = year(date),
    #        month = month(date, label = T, abbr = F))
    write_csv(uu, "../data/ABS/underutilisation.csv")
    
    return(uu)
    
  } else { uu <- read_csv("../data/ABS/underutilisation.csv") }
}

#Table 24 and 25 - Expanded underutilisation 
read_underutilisation_expanded <- function(force = F) {
  f_path <- "../data/ABS/underutilisation_expanded.csv"
  
  release <- data.frame(release_date = as.Date(c("2019-05-16", "2019-06-13", "2019-07-18", "2019-08-15", "2019-09-19", "2019-10-17"))) %>%
    mutate(released = ifelse(release_date <= today(), 1, 0))
  
  next_release <- release %>% 
    filter(released == 0) %>%
    pull(release_date) %>%
    min() 
  
  new_release <- next_release <= today()
  
  f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
  
  f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
  
  
  if(!f_exists | (!f_latest & new_release)) {
    
    uu_24 <- read_abs(cat_no = '6202.0', tables = 24, retain_files = F) %>%
      mutate(series = ifelse(str_detect(series, "years", negate = T), str_c("Total (age) ;", series), series)) %>%
      separate(series, into = c("age", "indicator", "gender"), sep = ';') %>%
      mutate(value = ifelse(unit == "000", (1000*value), (value)),
             year = year(date),
             month = month(date, label = T, abbr = F),
             state = "Australia") %>%
      mutate_at(.vars = c('indicator', 'gender', 'age'), trimws) %>%
      mutate_at(.vars = 'indicator', ~str_remove_all(., "> ")) %>%
      mutate_at(.vars = 'indicator', ~str_remove_all(., ">")) %>%
      select(date, month, year, indicator, gender, age, state, value, series_type, unit) 
    
    uu_25 <- read_abs(cat_no = '6202.0', tables = 25, retain_files = F) %>%
      separate_series(column_names = c("state", "indicator", "gender")) %>%
      mutate(value = ifelse(unit == "000", (1000*value), (value)),
             year = year(date),
             month = month(date, label = T, abbr = F),
             age = "Total (age)") %>%
      select(date, month, year, indicator, gender, age, state, value, series_type, unit)
    
    uu_e <- bind_rows(uu_24, uu_25)%>%
      mutate(id = str_c(date, month, year, indicator, gender, state, age, series_type))
    
    write_csv(uu_e, "../data/ABS/underutilisation_expanded.csv")
    
    return(uu_e)
    
  } else { uu_e <- read_csv("../data/ABS/underutilisation_expanded.csv") }
}

all_labour_force <- function(force = F) {
  bind_rows(read_labour_force(force),
            read_underutilisation(force),
            read_underutilisation_expanded(force)) %>%
    distinct(id, .keep_all = T) %>%
    mutate(month = factor(month, levels = month.name))

}

read_employment_by_industry <- function(force = F) {
  f_path <- "../data/employment_by_industry.csv"
  
  release <- data.frame(release_date = as.Date(c("2019-06-20", "2019-09-26"))) %>%
    mutate(released = ifelse(release_date <= today(), 1, 0))
  
  next_release <- release %>%
    filter(released == 0) %>%
    pull(release_date) %>%
    min()
  
  new_release <- next_release <= today()
  
  f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
  
  f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
  
  if(!f_exists | (!f_latest & new_release)) {
    
    emp_ind <- read_abs("6291.0.55.003", tables = 5) %>%
      separate(series,
               into=c("state", "industry", "employment_type"),
               sep=';',
               extra='drop') %>%
      mutate_at(vars("state", "industry", "employment_type"), ~str_remove_all(., "> ")) %>%
      mutate_if(is.character, trimws) %>%
      mutate(employment_type = ifelse(employment_type == "", industry, employment_type),
             industry = ifelse(str_detect(industry, "Employed"), "Total (industry)", industry),
             year = year(date),
             month = month(date, label = T, abbr = F),
             value = ifelse(unit == "000", value*1000, value)) %>%
      select(date, employment_type, industry, state, value, series_type, unit, year, month)
    
    write_csv(emp_ind, f_path)
    
    return(emp_ind)
  } else {emp_ind <- read_csv(f_path)}
  
}

# read_retail_trade <- function(force = F) {
#   f_path <- "/data/retail_trade.csv"
#   
#   release <- data.frame(release_date = as.Date(c("2019-05-07", "2019-06-04", "2019-07-02", "2019-08-06", "2019-09-03"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
#   
#   next_release <- release %>% 
#     filter(released == 0) %>%
#     pull(release_date) %>%
#     min() 
#   
#   new_release <- next_release <= today()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   
#   if(!f_exists | (!f_latest & new_release)) {
#   rt <- read_abs(cat_no = "8501.0", tables = 13) %>% 
#     separate(series,
#              into=c("type", "region", "industry"),
#              sep=';',
#              extra='drop') %>%
#     mutate_if(is.character, trimws) %>%
#     mutate(year = year(date),
#            month = month(date, label = T, abbr = F),
#            abs_series = "retail_trade",
#            region = ifelse(region == "Total (State)", "Australia", region)) %>%
#     select(abs_series, date, type, industry, region, value, series_type, unit, year, month)
#   
#   write_csv(rt, f_path)
#   
#   return(rt) 
#   
#   } else {rt <- read_csv(f_path)}
# }
# 

# 
# read_population <- function(force = F) {
#   f_path <- "www/data/population.csv"
#   
#   release <- data.frame(release_date = as.Date(c("2019-06-20", "2019-09-26"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
#   
#   next_release <- release %>% 
#     filter(released == 0) %>%
#     pull(release_date) %>%
#     min() 
#   
#   new_release <- next_release <= today()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   if(!f_exists | (!f_latest & new_release)) {
#     pop <- read_abs(cat_no = "3101.0", tables = 4) %>%
#       separate(series,
#                into=c("type", "gender", "region"),
#                sep=';',
#                extra='drop') %>%
#       mutate_if(is.character, trimws) %>%
#       mutate(year = year(date),
#              month = month(date, label = T, abbr = F),
#              abs_series = "population",
#              value = ifelse(unit == "000", value*1000, value)) %>%
#       select(abs_series, date, type, gender, region, value, series_type, unit, year, month)
#     
#     write_csv(pop, f_path)
#     
#     return(pop)
#   } else {pop <- read_csv(f_path)}
# }
# 
# read_population_pyramid <- function(force = F) {
#   f_path <- "www/data/population_pyramid.csv"
#   
#   release <- data.frame(release_date = as.Date(c("2019-06-20", "2019-09-26"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
#   
#   next_release <- release %>% 
#     filter(released == 0) %>%
#     pull(release_date) %>%
#     min() 
#   
#   new_release <- next_release <= today()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   if(!f_exists | (!f_latest & new_release)) {
#     pop_pyramid <- read_abs(cat = "3101.0", tables = c(51:59)) %>% 
#     separate(series, into = c("type", "gender", "age"), sep = ';', extra = 'drop') %>% 
#     mutate_if(is.character, trimws) %>%
#     mutate(year = year(date),
#            age = ifelse(age == "100 and over", "100", age),
#            age = as.numeric(age),
#            region = trimws(gsub(".*,", "", table_title)),
#            age_group = cut(age,breaks = c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,Inf),
#                            labels = c("0-4","5-9", "10-14","15-19","20-24","25-29","30-34","35-39", "40-44",
#                                       "45-49","50-54","55-59","60-64","65-69","70-74", "75-79", "80-84", "85-89",
#                                       "90-94","95-99","100+"))) %>%
#       arrange(age) %>%
#       mutate(age_group = factor(age_group, levels = unique(age_group))) %>%
#       select(date, type, gender, age, value, unit, year, region, age_group)
#   write_csv(pop_pyramid, f_path)
#   } else {pop_pyramid <- read_csv(f_path) %>% 
#     arrange(age) %>% 
#     mutate(age_group = factor(age_group, levels = unique(age_group)))
#   }
# }
# 
# read_exports_data <- function(force = F) {
#   f_path <- "www/data/exports_data.csv"
#   
#   release <- data.frame(release_date = as.Date(c("2019-06-20", "2019-09-26"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
#   
#   next_release <- release %>% 
#     filter(released == 0) %>%
#     pull(release_date) %>%
#     min() 
#   
#   new_release <- next_release <= today()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   if(!f_exists | (!f_latest & new_release)) {
#     exports_data <- read_abs(cat_no = "5368.0", tables = "36d") %>%
#       separate(series, into = c("destination"), sep = ';', extra = 'drop') %>%
#       mutate_at(vars(destination), trimws)
#     
#     write_csv(exports_data, f_path)
#     
#     return(exports_data)
#   } else {exports_data <- read_csv(f_path)}
#   
# }
# 
# read_national_accounts_annual <- function(force = F) {
#   f_path <- 'www/data/national_accounts_annual.csv'
#   
#   release <- data.frame(release_date = as.Date(c("2019-11-16"))) %>%
#     mutate(released = ifelse(release_date <=today(), 1, 0))
#   
#   next_release <- release %>%
#     filter(released == 0) %>%
#     pull(release_date) %>%
#     min()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   if(!f_exists | (!f_latest & new_release)) {
#     nat_accs_annual <- read_abs(cat_no = "5204.0", tables = 5) %>%
#       separate(series, into = c('sector', 'type'), sep = ';', extra = 'drop') %>%
#       mutate_if(is.character, trimws) %>%
#       mutate(year = year(date),
#              month = month(date, abbr = F, label = T)) %>% 
#       filter(type %in% c("Industry gross value added: Chain volume measures",
#                          "Gross state product: Chain volume measures")) %>%
#       separate(sector, into = c('sector', 'order'), sep = '\\(', extra = 'drop') %>%
#       mutate(order = gsub("\\)", "", order),
#              order = match(order, LETTERS),
#              type = ifelse(type == "Industry gross value added: Chain volume measures", "Industry Contribution", type),
#              type = ifelse(type == "Gross state product: Chain volume measures", "Gross State Product", type),
#              value = 1e6*value)
#     
#     write_csv(state_accounts, f_path)
#     return(state_accounts)
#   } else {state_accounts <- read_csv(f_path)}
# }
#   
# 
# 
# read_state_accounts <- function(force = F) {
#   f_path <- 'www/data/state_accounts.csv'
#   
#   release <- data.frame(release_date = as.Date(c("2019-11-16"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
#   
#   next_release <- release %>%
#     filter(released ==0) %>%
#     pull(release_date) %>%
#     min()
#   
#   new_release <- next_release <= today()
#   
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
#   
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
#   
#   if(!f_exists | (!f_latest & new_release)) {
#     state_accounts <- read_abs(cat_no = "5220.0", tables = 5) %>%
#       separate(series, into = c('sector', 'type'), sep = ';', extra = 'drop') %>%
#       mutate_if(is.character, trimws) %>%
#       mutate(year = year(date),
#              month = month(date, abbr = F, label = T)) %>% 
#       filter(type %in% c("Industry gross value added: Chain volume measures",
#                               "Gross state product: Chain volume measures")) %>%
#       separate(sector, into = c('sector', 'order'), sep = '\\(', extra = 'drop') %>%
#       mutate(order = gsub("\\)", "", order),
#              order = match(order, LETTERS),
#              type = ifelse(type == "Industry gross value added: Chain volume measures", "Industry Contribution", type),
#              type = ifelse(type == "Gross state product: Chain volume measures", "Gross State Product", type),
#              value = 1e6*value)
#    
#     write_csv(state_accounts, f_path)
#     return(state_accounts)
#   } else {state_accounts <- read_csv(f_path)}
# }
# 
# read_national_accounts <- function(force = F) {
#   f_path <- 'www/data/national_accounts.csv'
# 
#   release <- data.frame(release_date = as.Date(c("2019-05-06", "2019-09-04"))) %>%
#     mutate(released = ifelse(release_date <= today(), 1, 0))
# 
#   next_release <- release %>%
#     filter(released ==0) %>%
#     pull(release_date) %>%
#     min()
# 
#   new_release <- next_release <= today()
# 
#   f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))
# 
#   f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release
# 
#   if(!f_exists | (!f_latest & new_release)) {
#     nat_accounts <- read_abs(cat_no = '5206.0', tables = 1) %>%
#       select(date, series, value, series_type, unit) %>%
#       filter(str_detect(series, " - Percentage", negate = T)) %>%
#       mutate(series = gsub(" ;", "", series)) %>%
#       separate(series, into = c("type", "measure"), sep = ':', extra = 'drop') %>%
#       mutate_if(is.character, trimws) 
#     
#     write_csv(nat_accounts, f_path)
#     return(nat_accounts)
#   } else {nat_accounts <- read_csv(f_path)}
# }

#### Helpers for reporting ####  

date_breaks_format <- function(years) {
  switch(as.character(years),
         '1' = "3 months",
         '3' = "4 months", 
         '5' = "6 months",
         '10' = "1 year")
}

release_month <- function(data = all_lf, plus = 1) {
  data %>%
    filter(date == max(date)) %>%
    pull(date) %>%
    max() %>%
    `+`(months(plus)) %>%
    month(abbr = F, label = T)
}

growth <- function(indic = chunk_indicator, 
                   data = all_lf,  
                   ages = chunk_age,
                   region = chunk_region, 
                   series_type = "Trend", 
                   sex = chunk_gender, 
                   since = 2010) {
  data %>%
    filter(indicator == indic, 
           gender == sex,
           state == region, 
           age == chunk_age,
           series_type == ifelse(any(series_type == "Trend"), "Trend", "Original")) %>%
    group_by(year, indicator) %>%
    summarise(value = mean(value)) %>%
    filter(year == year(today()) | year == since) %>%
    ungroup() %>%
    mutate(pct = 100*(value-lag(value))/value,
           value = value-lag(value)) %>%
    filter(!is.na(value)) %>%
    select(value, pct)
}

last_month <- function(indic = chunk_indicator,
                       data = all_lf,
                       ages = chunk_age, 
                       region = chunk_region,
                       s_type = chunk_series_type, 
                       sex = chunk_gender,
                       year_max = max(all_lf$year),
                       period = c(0, -1)) {
  d <- data %>%
    filter(month %in% release_month(plus = period),
           indicator == indic,
           state == region,
           age == ages,
           gender == sex,
           year ==  year_max) %>%
    filter(series_type == ifelse(any(series_type == "Trend"), "Trend", "Original")) %>%
    arrange((date)) %>%
    pull(value)
  
  a <- c(abs(d[2]-d[1]), 100*abs(d[2]-d[1])/d[2])
  
  names(a) <- c("num", "pct")
  return(a)
           
}

last_year <- function(indic = chunk_indicator,
                       data = all_lf,
                       ages = chunk_age, 
                       region = chunk_region,
                       s_type = chunk_series_type, 
                       sex = chunk_gender,
                       month_max = release_month(plus = 0),
                       period = c(0, -1)) {
  d <- data %>%
    filter(month == month_max, 
           year %in% c(2019, 2018),
           indicator == indic,
           state == region,
           age == ages,
           gender == sex) %>%
    filter(series_type == ifelse(any(series_type == "Trend"), "Trend", "Original")) %>%
    arrange((date)) %>%
    pull(value)
  
  a <- c(abs(d[2]-d[1]), 100*abs(d[2]-d[1])/d[2])
  names(a) <- c("num", "pct")
  return(a)
  
}


  

value_at <- function(indic = chunk_indicator, 
                     data = all_lf,  
                     region = chunk_region, 
                     s_type = chunk_series_type,
                     ages = chunk_age, 
                     sex = chunk_gender, 
                     month_adjust = 0,
                     at = 2019) {
  data %>%
    filter(indicator == indic,
           gender == sex,
           state == region,
           month == release_month(plus = month_adjust),
           age == ages,
           series_type == s_type,
           year %in% at) %>%
    filter(series_type == ifelse(any(series_type == "Trend"), "Trend", "Original")) %>%
    ungroup() %>%
    pull(value)
}

value_at_month <-  function(indic = chunk_indicator, 
                            data = all_lf,  
                            region = chunk_region, 
                            series_type = chunk_series_type, 
                            ages = chunk_age,
                            sex = chunk_gender, 
                            at_year = max(all_lf$year),
                            at_month = release_month(all_lf, plus = 0)) {
  d <- data %>%
    filter(indicator == indic,
           gender == sex,
           state == region,
           age == ages) %>%
    filter(series_type == ifelse(any(series_type == "Trend"), "Trend", "Original")) %>%
    arrange(date) %>% 
    group_by(year, month, indicator) %>%
    summarise(value = mean(value)) %>%
    filter(year %in% at_year)
  

  d %>%
    filter(month %in% at_month) %>%
    ungroup() %>%
    pull(value)
}

as_comma <- function(string, 
                     suffix = NULL, 
                     neg_round = -2, ...) {
  suffix = as.character(suffix)
  if(all(round(string/1e6,1) < 1)) {
    str_c(formatC(round(string, neg_round), digits = 1, format = 'fg', big.mark = ',',  ...), suffix, sep = ' ')
  } else {
    str_c(formatC(string/1e6, format = 'f', big.mark = ',', digits = 2), 'million', suffix, sep = ' ')
  }
}

as_comma_group <- function(df,  
                           group = NULL, 
                           value = 'value', 
                           suffix = NULL, 
                           neg_round = -2,  
                           ...) {
  
  suffix <- as.character(suffix)
  groups <- df %>%
    pull(group) %>%
    unique() 
  
  a <- vector()
  
  for(i in groups) {
    string <- df %>% 
      filter(!!as.name(group) == i) %>% 
      pull(value)
    
    if(round(string[1]/1e6,1) < 1) {
      a <- append(a, str_c(formatC(round(string, neg_round), digits = 1, format = 'fg', big.mark = ',', ...), suffix, sep = ' '))
    } else {
      a <- append(a, str_c(formatC(string/1e6, format = 'f', big.mark = ',', digits = 2), 'million', suffix, sep = ' '))
    }
    
    
  }
  
  return(a)
}

as_percent <- function(string, 
                       scale=1,
                       ...) {
  
  str_c(formatC(string*scale, format = 'f', digits = 1, ...),"%")
  
}

current <- function(indic = chunk_indicator,
                    data = all_lf,  
                    region = chunk_region, 
                    series_type = chunk_series_type, 
                    sex = chunk_gender, 
                    ages = chunk_age) {
  data %>%
    filter(gender == sex,
           age == ages,
           indicator == indic,
           series_type == ifelse(any(series_type == "Trend"), "Trend", "Original"), 
           state == region) %>%
    filter(date == max(date)) %>%
    pull(value)
  
}

time_average <- function(indic = chunk_indicator, 
                         over, 
                         data = all_lf,  
                         region = chunk_region, 
                         series_type = "Trend", 
                         sex = chunk_gender,
                         ages = chunk_age) {
  over <- over/13
  data %>%
    filter(gender == sex,
           indicator == indic ,
           age == ages,
           series_type == ifelse(series_type == "Trend", "Trend", "Original"),
           state == region) %>%
    arrange(desc(date)) %>%
    slice(1:over*13) %>%
    summarise(value = mean(value)) %>%
    pull(value)
}

change <- function(period = chunk_period,
                   type = 'id',
                   value_1 = round(value_at(chunk_indicator, at = 2019, month_adjust = period[1]),1), 
                   value_2 = round(value_at(chunk_indicator, at = 2018, month_adjust = period[2]),1)) {
  

  if(type == "id") {
    case_when(value_1 > value_2 ~ 'increased',
              value_1 < value_2 ~ 'decreased',
              value_1 == value_2 ~ 'remained steady at')
  } else if(type == "ab") {
    case_when(value_1 > value_2 ~ 'above',
              value_1 < value_2 ~ 'below',
              value_1 == value_2 ~ 'the same as')
  } else if(type == 'rf') {
    case_when(value_1 > value_2 ~ 'risen',
              value_1 < value_2 ~ 'fallen',
              value_1 == value_2 ~ 'remained steady at')
  } else if(type == 'present') {
    case_when(value_1 > value_2 ~ 'an increase',
              value_1 < value_2 ~ 'a decrease',
              value_1 == value_2 ~ 'steady')
  }
}

change_month <- function(period = chunk_period,
                         type = 'id') {
  
  value_1 <- round(value_at_month(chunk_indicator, at_month = release_month(plus = period[1])),1)
  value_2 <- round(value_at_month(chunk_indicator, at_month = release_month(plus = period[2])),1)

  
  if(type == "id") {
    case_when(value_1 > value_2 ~ 'increased by',
              value_1 < value_2 ~ 'decreased by',
              value_1 == value_2 ~ 'remained at')
  } else if(type == "ab") {
    case_when(value_1 > value_2 ~ 'above',
              value_1 < value_2 ~ 'below',
              value_1 == value_2 ~ 'the same as')
  } else if(type == 'rf') {
    case_when(value_1 > value_2 ~ 'risen',
              value_1 < value_2 ~ 'fallen',
              value_1 == value_2 ~ 'remained')
  } else if(type == 'present') {
    case_when(value_1 > value_2 ~ 'an increase', 
              value_1 < value_2 ~ 'a decrease',
              value_1 == value_2 ~ 'the same level as last month')
  }
}

