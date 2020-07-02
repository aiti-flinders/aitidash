library(readabs)
read_labour_force <- function(force = F) {
f_path <- "www/data/labour_force.csv"

if(force) {
lf <- read_abs(cat = "6202.0", tables = c(12,23), retain_files = FALSE) %>%
  mutate(series = str_extract(series, ".+(?= ;)"),
         series = str_replace_all(series, pattern = ">", replacement = ""),
         series = str_squish(series)) %>%
  separate(series, 
           into = c("indicator", "gender", "region"), 
           sep = ';', 
           remove = FALSE, 
           fill = 'right') %>%
  mutate_at(.vars = c("indicator", "gender", "region"), str_squish) %>%
  mutate(region = ifelse(is.na(region), "Australia", region),
         value = ifelse(unit == "000", value*1000, value),
         year = year(date),
         month = month(date, label = TRUE, abbr = FALSE)) %>%
  distinct(date, indicator, gender, region, value, series_type, unit, year, month) %>%
  pivot_wider(names_from = indicator, values_from = value) %>% 
  mutate(`Underutilised total` = `Underemployed total` + `Unemployed total`) %>%
  pivot_longer(cols = c(8:26), names_to = 'indicator', values_to = 'value', values_drop_na = TRUE)

write_csv(lf, "www/data/labour_force.csv")

} else {
  
  lf <- read_csv(f_path)
  
}

return(lf)

} 


read_retail_trade <- function(force = F) {
  f_path <- "www/data/retail_trade.csv"
  
  
  if(force) {
    rt <- read_abs(cat_no = "8501.0", tables = 13, retain_files = F) %>% 
      mutate(series = str_extract(series, ".+(?= ;)"),
             series = str_replace_all(series, pattern = ">", replacement = ""),
             series = str_squish(series)) %>%
      separate(series,
               into=c("indicator", "region", "sector"),
               sep=';',
               fill = 'left', 
               remove = FALSE) %>%
      mutate_at(.vars = c("region", "sector"), str_squish) %>%
      mutate(year = year(date),
             month = month(date, label = TRUE, abbr = F),
             region = ifelse(region == "Total (State)", "Australia", region)) %>%
      distinct(date, region, sector, value, series_type, unit, year, month)  
    
    write_csv(rt, f_path)
    
    return(rt) 
    
  } else {
    rt <- read_csv(f_path)
  }
  
}

read_employment_by_industry <- function(force = F) {
  f_path <- "www/data/employment_by_industry.csv"
  
  if(force) {
    
    emp_ind <- read_abs("6291.0.55.003", tables = 5, retain_files = FALSE) %>% 
      separate_series(column_names = c("region", "industry", "indicator"),
                      remove_nas = TRUE) %>%
      mutate(year = year(date),
             month = month(date, label = T, abbr = F),
             value = ifelse(unit == "000", value*1000, value)) %>%
      select(date, industry, region, series_type, unit, year, month, indicator, value) %>%
      group_by(date, industry, region, indicator) %>%
      mutate(share = value/sum(value)) %>%
      ungroup()
    
    write_csv(emp_ind, f_path)
    
    return(emp_ind)
  } else {
    emp_ind <- read_csv(f_path)
  }
  
}

read_population <- function(force = F) {
  
  f_path <- 'www/data/population.csv'
  
  if(force) {
    pop <- read_abs(cat_no = "3101.0", tables = 4, retain_files = FALSE) %>%
      separate_series(column_names = c("indicator", "gender", "region")) %>%
      mutate(year = year(date),
             month = month(date, label = T, abbr = F),
             value = ifelse(unit == "000", value*1000, value)) %>%
      select(date, gender, region, series_type, unit, year, month, indicator,  value)
    
    write_csv(pop, f_path)
    
    return(pop)
  } else {
    pop <- read_csv(f_path)
  }
}

read_population_pyramid <- function(force = F) {
  f_path <- "www/data/population_pyramid.csv"
  
  if(force) {
    pop_pyramid <- read_abs(cat = "3101.0", tables = 51:59, retain_files = FALSE) %>% 
    separate_series(column_names = c('indicator', 'gender', 'age')) %>% 
    mutate(year = year(date),
           age = ifelse(age == "100 and over", "100", age),
           age = as.numeric(age),
           region = trimws(gsub(".*,", "", table_title)),
           age_group = cut(age,
                           breaks = c(-Inf,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,99,Inf),
                           labels = c("0-4","5-9", "10-14","15-19","20-24","25-29","30-34","35-39", "40-44",
                                      "45-49","50-54","55-59","60-64","65-69","70-74", "75-79", "80-84", "85-89",
                                      "90-94","95-99","100+"))) %>%
      arrange(age) %>%
      mutate(age_group = factor(age_group, levels = unique(age_group))) %>%
      select(date, indicator, gender, age, value, unit, year, region, age_group)
    
  write_csv(pop_pyramid, f_path)
  
  } else {
    pop_pyramid <- read_csv(f_path)
  }
}

read_exports_data <- function(force = F) {
  f_path <- "www/data/exports_data.csv"
  
  
  
  if(force) {
    exports_data <- read_abs(cat_no = "5368.0", tables = "36d") %>%
      separate(series, into = c("destination"), sep = ';', extra = 'drop') %>%
      mutate_at(vars(destination), trimws)
    
    write_csv(exports_data, f_path)
    
    return(exports_data)
  } else {exports_data <- read_csv(f_path)}
  
}

read_national_accounts_annual <- function(force = F) {
  f_path <- 'www/data/national_accounts_annual.csv'
  
  if(force) {
    nat_accs_annual <- read_abs(cat_no = "5204.0", tables = 5, retain_files = FALSE) %>%
      separate_series(column_names = c('industry', 'indicator')) %>%
      mutate(year = year(date),
             month = month(date, abbr = F, label = T)) %>%
      separate(indicator, into = c('sub_industry', 'indicator'), 
               sep = ':', 
               extra = 'drop') %>%
      filter(!is.na(indicator),
             indicator != "Chain volume measures",
             !is.na(value)) %>%
      mutate(value = 1e6*value) %>%
      select(date,industry, sub_industry, value, series_type, unit, year, month)
    
    write_csv(nat_accs_annual, f_path)
    
    return(nat_accs_annual)
  } else {
    nat_accs_annual <- read_csv(f_path)
    }
}
  
read_state_accounts <- function(force = F) {
  f_path <- 'www/data/state_accounts.csv'

  if(force) {
    state_accounts <- read_abs(cat_no = "5220.0", tables = 5, retain_files = FALSE) %>%
      separate_series(column_names = c('industry', 'indicator')) %>%
      mutate(year = year(date),
             month = month(date, abbr = F, label = T)) %>% 
      separate(indicator, into = c('indicator', 'measure'),
               sep = ":",
               extra ='drop') %>%
      filter(!is.na(value)) %>%
      mutate(value = ifelse(unit == "$ Millions", value*1e6, value))
   
    write_csv(state_accounts, f_path)
    return(state_accounts)
  } else {
    state_accounts <- read_csv(f_path)
    }
}

read_national_accounts <- function(force = F) {
  f_path <- 'www/data/national_accounts.csv'

  release <- data.frame(release_date = as.Date(c("2019-05-06", "2019-09-04"))) %>%
    mutate(released = ifelse(release_date <= today(), 1, 0))

  next_release <- release %>%
    filter(released ==0) %>%
    pull(release_date) %>%
    min()

  new_release <- next_release <= today()

  f_exists <- ifelse(force, !file.exists(f_path), file.exists(f_path))

  f_latest <- format(file.info(f_path)$mtime, "%Y-%m-%d") <= next_release

  if(!f_exists | (!f_latest & new_release)) {
    nat_accounts <- read_abs(cat_no = '5206.0', tables = 1) %>%
      select(date, series, value, series_type, unit) %>%
      filter(str_detect(series, " - Percentage", negate = T)) %>%
      mutate(series = gsub(" ;", "", series)) %>%
      separate(series, into = c("type", "measure"), sep = ':', extra = 'drop') %>%
      mutate_if(is.character, trimws) 
    
    write_csv(nat_accounts, f_path)
    return(nat_accounts)
  } else {nat_accounts <- read_csv(f_path)}
}

  