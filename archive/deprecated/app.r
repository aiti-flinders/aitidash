library(shinydashboard)
library(tidyverse)
library(readabs)
library(lubridate)
library(plotly)
library(scales)
theme_set(theme_bw()+ 
            theme(
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour='black'),
              legend.spacing.x = unit(0.3, 'cm'),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
            ))

#### Data Setup ####
nth_day_of_month <- function(n, day, start, finish) {
  x <- seq(ymd(start), ymd(finish), by = "1 day")
  return(x[wday(x, label = T) == day & day(x) > 7*(n-1) & day(x) <= 7*n])
}


#Labour Force Survey is released the 3rd thursday of every month (mostly?)
lf_release <- data.frame(release_date = nth_day_of_month(3, "Thu", "2019-03-01", "2019-09-30")) %>%
  mutate(released = ifelse(today() >= release_date, 1, 0))

#Retail Trade Survey is released the first tuesday of every month (kind of)
rt_release <- data.frame(release_date = nth_day_of_month(1, "Tue", "2019-03-01", "2019-09-30")) %>%
  mutate(released = ifelse(today() >= release_date, 1, 0))

lf_q_release <- data.frame(release_date = as.Date(c("2019-03-28","2019-06-20", "2019-09-26"))) %>%
  mutate(released = ifelse(today() >= release_date, 1, 0))

check_schedule <- function(release) {
  switch(release,
         "labour_force" = today() >= lf_release %>% filter(released == 0) %>% .$release_date %>% min(),
         "retail_trade" = today() >= rt_release %>% filter(released == 0) %>% .$release_date %>% min(),
         "labour_force_qrt" = today () >= lf_q_release %>% filter(released == 0) %>% .$release_date %>% min())
                            
}

wrangle_abs <- function(cat, tables, release) {
  #Release date logic:
  #If there is a local file and todays date is before the next release date: Read from local
  #Else download, and update the release calendar
  f_path <- str_c("./data/ABS/",str_sub(cat, 1, 4), 0, tables, '.xls') 
  downloaded <-  file.exists(f_path)
  new_release <-  check_schedule(release)
  if(downloaded & !new_release) {
    df <- read_abs_local(str_c(str_sub(cat, 1, 4), 0, tables, '.xls'))
  } else {
    df <- read_abs(cat_no = cat, tables = tables) 
    lf_release <- lf_release %>%
      filter(release_date <= today()) %>%
      mutate(released = 1)
  }
  df <- df %>%
    separate(series,
             into=c("type", "gender", "region"),
             sep=';',
             extra='drop') %>%
    mutate_at(vars("type", "gender", "region"), ~str_remove_all(., "> ")) %>%
    mutate_at(vars("type"), str_to_title) %>%
    mutate_if(is.character, trimws) %>%
    mutate(year = year(date),
           month = month(date, label = T, abbr = F),
           abs_series = "labour_force",
           value = ifelse(unit == "000", value*1000, value)) %>%
    select(abs_series, date, type, gender, region, value, series_type, unit, year, month)
}



# Labour Force
lf <- wrangle_abs(cat = "6202.0", tables = 12, release = "labour_force")
#Retail Trade
rt <- wrangle_abs(cat = "8501.0", tables = 3, release = "retail_trade") %>%
  rename(region = gender, industry = region)
#Underutilisation
uu <- wrangle_abs(cat = "6202.0", tables = 23, release = "labour_force") %>%
  mutate(region = ifelse(region == "", "Australia", region))

lf <- rbind(lf, uu) %>% 
  distinct()

#Employment by Industry
emp_ind <- wrangle_abs(cat = "6291.0.55.003", tables = 5, release = "labour_force_qrt") %>%
  rename(region = type, industry = gender, type = region) %>%
  filter(type != "")



#Create a bunch of helper functions for reporting text

last_month <- function(data, series_name, region_name, sex, s_type) {
  df <- data %>%
    filter(type == series_name,
           region %in% region_name,
           gender == sex,
           series_type == s_type)
  if(month(max(df$date))==1) {
    #For january releases, the previous month will be december of the previous year
    df_a <- df %>%
      filter(month == month(12, abbr = F, label = T),
             year == year(max(df$date)-1))
    df_b <- df %>%
      filter(month == month(1, abbr = F, label = T),
             year == year(max(df$date)))
    
    df <- rbind(df_a, df_b) %>%
      .$value
    
    df <- set_names(c((df[2]-df[1]), 100*(df[2]-df[1])/df[1]), c("num", "pct"))
  } else {
    df <- df %>% 
      filter(year == year(max(df$date)),
             month == month(max(df$date), abbr = F, label = T) | month == month(max(df$date)-1, abbr = F, label = T)) %>%
      .$value
    
    df <- set_names(c((df[2]-df[1]), 100*(df[2]-df[1])/df[1]), c("num", "pct"))
  }
  
}

last_year <- function(data, series_name, region_name, sex, s_type) {
  df <- data %>%
    filter(region %in% region_name, 
           type == series_name,
           gender == sex,
           series_type == s_type,
           month == month(max(date), abbr = F, label = T),
           year == year(max(date)) | year == year(max(date))-1) %>%
    .$value
  
  df <- set_names(c((df[2]-df[1]), 100*(df[2]-df[1])/df[1]), c("num", "pct"))
  
}

current <- function(data, series_name, region_name, sex, s_type) {
  df <- data %>%
    filter(type == series_name,
           region %in% region_name,
           gender == sex,
           series_type == s_type,
           date == max(date))
  
  df <- df$value
  
}

release_month <- function(data) {
  df <- data %>%
    filter(date == max(date)) %>%
    select(date) %>%
    .[[1]] %>%
    .[1] %>% 
    month(abbr = F, label = T)
}

incr_decr <- function(data, time, indic, s_type) {
  switch(time,
         "month" = ifelse(last_month(data = data, series_name = indic , region_name = "South Australia", sex = "Persons", s_type = s_type)['num'] >0,
                          "increased", 
                          "decreased"),
         "year" = ifelse(last_year(data = data, series_name = indic , region_name = "South Australia", sex = "Persons", s_type = s_type)['num'] >0,
                         "increased", 
                         "decreased")
  )
}

analysis <- function(data, indic, s_type) {
  
  df <- data %>% 
    filter(type == indic,
           series_type == s_type)
  
  analysis_text <- str_c(sep = ' ',
                         "In",
                         release_month(data),
                         "the",
                         s_type,
                         indic,
                         incr_decr(data, time = "month", indic = indic, s_type = s_type),
                         "by",
                         last_month(data, indic, region_name = "South Australia", sex = "Persons", s_type = s_type)['num'] %>%
                           formatC(format = 'f', digits = 1))
  HTML(analysis_text)
}


#Plotly Setup
Sys.setenv("plotly_username"="hamgamb")
Sys.setenv("plotly_api_key" = 'SDYMDyK3YM0eZrTNpyoa')



#### Header Controls ####
header = dashboardHeader(title = NULL, titleWidth = 200)

##### Sidebar Controls #####
sidebar = dashboardSidebar(width = 200, 
                           sidebarMenu(
                             menuItem(
                               text="Labour Market",  
                               tabName = "labour_market",
                               icon = icon("user-tie")
                             ),
                             menuItem(
                               text="Industry",
                               tabName = "industry",
                               icon = icon('cogs')
                             ),
                             menuItem(
                               text="Consumer Activity",
                               tabName = 'consumer_activity', 
                               icon = icon('credit-card')
                             ),
                             menuItem(
                               text="Demographics",
                               tabName = 'demographics',
                               icon = icon('user-plus')
                             ),
                             menuItem(
                               text="Economic Output",
                               tabName = 'economic_output',
                               icon = icon('dollar-sign')
                             )))
#### Labour Market ####
#### Time Series Tab Panel #### (Finished)
time_series_tab_panel_labour_market = tabPanel(title = "South Australia", plotlyOutput("time_series_plot_lm", width='100%'),
                                               fluidRow(
                                                 box(status ='info',solidHeader = TRUE,
                                                     selectInput(
                                                       inputId = "tstp_lm_indicator",
                                                       label = "Select Labour Market Indicator", 
                                                       choices = c("Employed Total", "Unemployment Rate", "Employment To Population Ratio", "Underutilisation Rate"),
                                                       selected = "Unemployment Rate"),
                                                     selectInput(
                                                       inputId = 'tstp_lm_series_type', 
                                                       label = "Select Series Type", 
                                                       choices = sort(unique(lf$series_type)), 
                                                       selected = "Original"),
                                                     sliderInput(
                                                       inputId = 'tstp_lm_date_range',
                                                       label = "Select Date Range",
                                                       min = min(lf$date),
                                                       max = max(lf$date),
                                                       value = c(min(lf$date),  max(lf$date)),
                                                       timeFormat = "%Y-%b"
                                                     )),
                                                 box(status='info',solidHeader = TRUE,
                                                     tableOutput('time_series_table_lm')),
                                                 box(status='info', solidHeader = TRUE,
                                                     uiOutput('time_series_analysis_lm'))
                                               ))
#### Regional Tab Panel ####
regional_tab_panel_labour_market = tabPanel(title = "Region", plotlyOutput("region_plot", width='100%'),
                                            fluidRow(
                                              box(status= 'info',solidHeader=TRUE,
                                                  selectInput(
                                                    inputId = 'rtp_indicator', 
                                                    label = "Select Labour Market Indicator",
                                                    choices = c("Employed Total", "Unemployment Rate", "Employment To Population Ratio"),
                                                    selected = "Unemployment Rate"),
                                                  selectInput(
                                                    inputId = 'rtp_series_type',
                                                    label =  "Select Series Type", 
                                                    choices = c("Original", "Trend"),
                                                    selected = "Trend"),
                                                  numericInput(
                                                    inputId = 'rtp_index_date',
                                                    label = 'Select Base Year',
                                                    value = 2010,
                                                    min = min(lf$year),
                                                    max = max(lf$year),
                                                    step = 1)),
                                              box(status='info',solidHeader=TRUE,
                                                  checkboxGroupInput(
                                                    inputId = 'rtp_state_select',
                                                    label = "Select States",
                                                    choices = unique(lf$region))
                                                  
                                              ),
                                              box(status='info', solidHeader=TRUE,
                                                  uiOutput(outputId = 'region_analysis'))
                                            )
)

#### Demographic Tab Panel ####
demographic_tab_panel_labour_market = tabPanel(title = "Demographic", plotlyOutput("demographic_plot", width='100%'),
                                               fluidRow(
                                                 box(status='info', solidHeader=TRUE,
                                                     selectInput(
                                                       inputId = 'dtp_indicator',
                                                       label = "Select Labour Market Indicator",
                                                       choices = c("Employed Total", "Unemployment Rate", "Employment To Population Ratio"),
                                                       selected = "Unemployment Rate"),
                                                     selectInput(
                                                       inputId = 'dtp_baseline',
                                                       label = "Select Comparison Year",
                                                       choices = sort(unique(lf$year[lf$year != 2018]), decreasing = T),
                                                       selected = 2017)
                                                 ),
                                                 box(status='info', solidHeader=TRUE,
                                                     tableOutput(outputId = 'demographic_table')),
                                                 box(status='info', solidHeader=TRUE,
                                                     uiOutput(outputId = 'demographic_analysis'))
                                               )
)
#### Labour Market Tab ####
labour_market_tab = tabItem(tabName = 'labour_market', 
                            fluidRow(
                              infoBoxOutput("unemployment_box", width = 6),
                              infoBoxOutput("employment_box", width = 6)
                            ),
                            fluidRow(
                              infoBoxOutput("employment_population_box", width = 6),
                              infoBoxOutput("underutilisation_box", width = 6)
                            ),
                            fluidRow(
                              tabBox(width = 12,
                                     time_series_tab_panel_labour_market,
                                     regional_tab_panel_labour_market,
                                     demographic_tab_panel_labour_market)
                            )
)

#### Consumer Activity ####
#### Time Series Tab Panel ####
time_series_tab_panel_consumer_activity = tabPanel(title = "South Australia", plotlyOutput("time_series_plot_ca", width='100%'),
                                                   fluidRow(
                                                     box(status = 'info', collapsible = T,solidHeader = TRUE,
                                                         selectInput(
                                                           inputId = "tstp_ca_indicator",
                                                           label = "Select Consumer Activity Indicator", 
                                                           choices = c("Turnover"),
                                                           selected = "Turnover"),
                                                         selectInput(
                                                           inputId = "tstp_ca_series_type",
                                                           label = "Select Series Type",
                                                           choices = sort(unique(rt$series_type)),
                                                           selected = "Original"),
                                                         sliderInput(
                                                           inputId = 'tstp_ca_date_range',
                                                           label = "Select Date Range",
                                                           min = min(rt$date),
                                                           max = max(rt$date),
                                                           value = c(min(rt$date), max(rt$date))
                                                         )),
                                                     box(status='info',solidHeader = TRUE,
                                                         tableOutput('time_series_table_ca')),
                                                     box(status='info', solidHeader = TRUE,
                                                         uiOutput('time_series_analysis_ca'))
                                                   ))


consumer_activity_tab = tabItem(tabName = 'consumer_activity',
                                fluidRow(
                                  infoBoxOutput("retail_trade_box", width=4)
                                ),
                                fluidRow(
                                  tabBox(width = 12,
                                         time_series_tab_panel_consumer_activity
                                  )
                                )
)

demographics_tab = tabItem(tabName = 'demographics',
                           fluidRow(
                             infoBoxOutput("population_box", width=4)
                           )
)

economic_output_tab = tabItem(tabName = 'economic_output',
                              fluidRow(
                                infoBoxOutput("gsp_box", width=4)
                              )
)





body <- dashboardBody(
  tags$style(
    type = 'text/css', 
    '.bg-green {background-color: #72CB72!important; }',
    '.bg-red {background-color: #CB7272!important; }',
    '.bg-blue {background-color: #006ab6!important; }'
  ),
  tabItems(
    labour_market_tab,
    consumer_activity_tab,
    demographics_tab,
    economic_output_tab
  )
)



#### Industry ####
#### Employment by Industry Panel ####
emp_ind_sa_panel = tabPanel(title = "South Australia", plotlyOutput("emp_ind_sa_plot", width='100%'),
                            fluidRow(
                              box(status = 'info', collapsible = T,solidHeader = TRUE,
                                  selectInput(
                                    inputId = "tstp_ca_indicator",
                                    label = "Select Consumer Activity Indicator", 
                                    choices = c("Turnover"),
                                    selected = "Turnover"),
                                  selectInput(
                                    inputId = "tstp_ca_series_type",
                                    label = "Select Series Type",
                                    choices = sort(unique(rt$series_type)),
                                    selected = "Original"),
                                  sliderInput(
                                    inputId = 'tstp_ca_date_range',
                                    label = "Select Date Range",
                                    min = min(rt$date),
                                    max = max(rt$date),
                                    value = c(min(rt$date), max(rt$date))
                                  )),
                              box(status='info',solidHeader = TRUE,
                                  tableOutput('time_series_table_ca')),
                              box(status='info', solidHeader = TRUE,
                                  uiOutput('time_series_analysis_ca'))
                            ))

employment_by_industry_tab = tabItem(tabName = 'emp_ind',
                                     fluidRow(
                                       tabBox(width = 12,
                                              emp_ind_sa_panel)
                                     )
)

#### UI ####
ui <- dashboardPage(
  header,
  sidebar,
  body
  
)

#### Server ####
server <- function(input, output, session) {
  choices <- reactive({
    lf <- lf %>% filter(type == input$rtp_indicator)
    lf <- sort(unique(lf$series_type))
  })
  
  data = reactive({
    df = df_all %>% filter(abs_ser)
  })
  
  
  output$emp_ind_sa_plot <- renderPlotly({
    #Total Employment
    emp_ind %>%
      filter(region == "South Australia", type == "Employed total") %>% 
      group_by(year, industry) %>% 
      summarise(value = mean(value)) %>% 
      filter(year == 2019)  %>% 
      ggplot(aes(x=reorder(industry, value), y = value)) + 
      geom_bar(stat='identity') + 
      labs(
        y = "# Employed",
        x = NULL
      ) +
      scale_y_continuous(labels = comma_format(scale = 1)) +
      coord_flip()
  })
  
  # #Full/Part Time 
  # emp_ind %>%
  #   filter(region == "South Australia", type != "Employed total") %>% 
  #   group_by(year, type, industry) %>% 
  #   summarise(value = mean(value)) %>% 
  #   ungroup() %>% 
  #   filter(year == 2019)  %>% 
  #   ggplot(aes(x=reorder(industry, value), y = value, fill = type)) + 
  #   geom_bar(stat='identity') + 
  #   labs(
  #     y = "# Employed",
  #     x = NULL
  #   ) +
  #   scale_y_continuous(labels = comma_format(scale = 1)) +
  #   coord_flip()
  # 
  # #Shift-Share 2009-2019
  # emp_ind %>%
  #   filter(region == "South Australia", type == "Employed total") %>% 
  #   group_by(year, type, industry) %>% 
  #   summarise(value = mean(value)) %>% 
  #   ungroup() %>% 
  #   group_by(year, type) %>%
  #   mutate(share = 100*value/sum(value)) %>% 
  #   filter(year == 2009 | year == 2019) %>%
  #   select(industry, year, share) %>%
  #   spread(key = year, value = share, sep = "_") %>%
  #   mutate(shift_share = year_2019 - year_2009) %>%
  #   ggplot(aes(x=reorder(industry, shift_share), y = shift_share)) + 
  #   geom_bar(stat='identity') + 
  #   labs(
  #     y = NULL,
  #     x = NULL
  #   ) +
  #   scale_y_continuous(labels = percent_format(scale = 1)) +
  #   coord_flip()
  
  
  output$time_series_table_lm <- renderTable(align = 'c', expr = {
    lf %>%
      filter(month == "February",
             type == input$tstp_lm_indicator,
             region == "South Australia",
             series_type == "Trend",
             gender == "Persons",
             year == max(lf$year) | year == max(lf$year)-1) %>%
      select(year, value) %>%
      spread(key = year, value = value) %>%
      mutate(`% Change` = `2019`-`2018`)
      
  })
  
  output$time_series_analysis_lm <- renderUI({
    analysis(data = lf, indic = input$tstp_lm_indicator, s_type = input$tstp_lm_series_type)
    # HTML(str_c("ABS Labour Force Survey 6202.0<br>",
    #       "Released ", 
    #       lf_release %>% 
    #         filter(released == 1) %>% 
    #         .$release_date %>%
    #         max() %>%
    #         format("%B %d %Y"),
    #       "In", release_month(lf), input$tstp_lm_series_type, input$tstp_lm_indicator, 
    #       "was"))
    })
  
  output$unemployment_box <- renderInfoBox({
    infoBox(
      "Unemployment Rate: ",
      str_c(current(lf, "Unemployment Rate", "South Australia", "Persons", "Trend") %>% 
              formatC(format = 'f', digits = 1), "%"),
      icon = icon(
        ifelse(
          last_year(lf, "Unemployment Rate", "South Australia", "Persons", "Trend")['num'] < 0,
          'arrow-alt-circle-down',
          'arrow-alt-circle-up'
        )
      ),
      fill = T,
      subtitle = str_c(
        "Year on year: ",
        last_year(lf, "Unemployment Rate", "South Australia", "Persons", "Trend")['num'] %>% 
          formatC(format = 'f', digits = 1), "% pt"),
      color = ifelse(
        last_year(lf, "Unemployment Rate", "South Australia", "Persons", "Trend")['num'] <0,
        "green",
        "red"
      ),
      width = 4
    )
  })
  
  output$employment_box <- renderInfoBox({
    infoBox("Total Employment: " ,
            current(lf, "Employed Total", "South Australia", "Persons", "Trend") %>% 
              formatC(format = 'f', digits = 0, big.mark = ','),
            icon = icon(
              ifelse(
                last_year(lf, "Employed Total", "South Australia", "Persons", "Trend")['num'] > 0,
                'arrow-alt-circle-up',
                'arrow-alt-circle-down'
              )
            ),
            fill = T,
            subtitle = str_c(
              "Year on year: ",
              last_year(lf, "Employed Total", "South Australia", "Persons", "Trend")['num'] %>% 
                formatC(format = 'f', digits = 0, big.mark = ','), " persons"),
            color = ifelse(
              last_year(lf, "Employed Total", "South Australia", "Persons", "Trend")['num'] > 0,
              "green",
              "red"
            ),
            width = 4)
  })
  
  output$employment_population_box <- renderInfoBox({
    infoBox(title = "Employment to Population Ratio: ",
            value = str_c(
              current(lf, "Employment To Population Ratio", "South Australia", "Persons", "Trend") %>% 
                formatC(format = 'f', digits = 1), "%"),
            icon = icon(
              ifelse(
                last_year(lf, 'Employment To Population Ratio', "South Australia", "Persons", "Trend")['num'] > 0,
                'arrow-alt-circle-up',
                'arrow-alt-circle-down'
              )
            ),
            fill = T,
            subtitle = str_c(
              "Year on year: ",
              last_year(lf, "Employment To Population Ratio", "South Australia", "Persons", "Trend")['num'] %>% 
                formatC(format = 'f', digits = 1), "% pt"),
            color = ifelse(
              last_year(lf, 'Employment To Population Ratio', "South Australia", "Persons", "Trend")['num'] > 0,
              "green",
              "red"
            ))
  })
  
  output$underutilisation_box <- renderInfoBox({
    infoBox(title = "Underutilisation Rate: ",
            value = str_c(
              current(lf, "Underutilisation Rate", "South Australia", "Persons", "Trend") %>%
                formatC(format = 'f', digits = 1), "%"),
            icon = icon(
              ifelse(
                last_year(lf, "Underutilisation Rate", "South Australia", "Persons", "Trend")['num'] > 0,
                'arrow-alt-circle-up',
                'arrow-alt-circle-down'
              )
            ),
            fill = T,
            subtitle = str_c(
              "Year on year: ",
              last_year(lf, "Underutilisation Rate", "South Australia", "Persons", "Trend")['num'] %>%
                formatC(format = 'f', digits = 1), "% pt"),
            color = ifelse(
              last_year(lf, "Underutilisation Rate", "South Australia", "Persons", "Trend")['num'] > 0,
              'red',
              'green'
            ))
          
  })
  
  output$retail_trade_box <- renderInfoBox({
    infoBox("Retail Trade",
            "1 Billion",
            icon = icon('credit-card'))
  })
  
  output$population_box <- renderInfoBox({
    infoBox("Population",
            "1.7 Million",
            icon=icon('user-plus'))
  })
  
  output$gsp_box <- renderInfoBox({
    infoBox("Gross State Product",
            "100 Billion",
            icon = icon('dollar-sign'))
  })
  
  output$time_series_plot_lm <- renderPlotly({
    df = lf %>% filter(
      type == input$tstp_lm_indicator,
      gender == "Persons",
      region == "South Australia",
      series_type == input$tstp_lm_series_type,
      date >= input$tstp_lm_date_range[1],
      date <= input$tstp_lm_date_range[2]
    )
    
    p = ggplot(df, aes(x = date, 
                       y = value,
                       text = paste0('Date: ', format(date, "%Y-%b"),
                                    '<br>',input$tstp_lm_indicator,': ', 
                                    if(df$unit[1]=="Percent"){str_c(formatC(value, format = 'f', digits = 1), "%")}
                                    else{str_c(formatC(value, format = 'f', digits = 0, big.mark=","))}),
                       group = 1)) +
      geom_line(colour="#fde06d", size=0.5) +
      labs(
        x = NULL,
        y=input$tstp_lm_indicator,
        #title = input$tstp_lm_indicator,
        #subtitle = input$region,
        caption = "Source: ABS: 6202.0"
      ) +
      scale_color_manual(name = NULL,
                         values = c("#56B4E9"),
                         breaks = c("Persons"),
                         labels = c("Persons"),
                         drop = FALSE) +
      theme(legend.position = 'none',
            axis.text.x=element_text(angle=90, vjust=0.5, size=8),
            panel.grid.minor = element_blank()) +

      if(df$unit[1]=="Percent"){label = scale_y_continuous(label=function(x) paste0(x,"%"))}
      else{scale_y_continuous(label = comma_format(suffix = "k",scale = 1/1000))}
    ggplotly(p, tooltip='text')
    

    
  })
  
  output$time_series_plot_ca <- renderPlotly({
    df = rt %>% filter(
      type == input$tstp_ca_indicator,
      industry == "Total (Industry)",
      region == "South Australia",
      date >= input$tstp_ca_date_range[1],
      date <= input$tstp_ca_date_range[2],
      series_type == input$tstp_ca_series_type
    )
    
    p = ggplot(df, aes(x = date, 
                       y = value)) +
      geom_line() +
      labs(
        x = NULL,
        y=input$tstp_ca_indicator,
        #title = input$tstp_lm_indicator,
        #subtitle = input$region,
        caption = "Source: ABS: 6202.0"
      ) +

      
      theme(legend.position = 'none') + 
   scale_y_continuous(label = comma_format(scale = 1000))
    ggplotly(p)
    
    
    
  })
  
  output$region_plot <- renderPlotly({
    df = lf %>% filter(
      type == input$rtp_indicator,
      series_type == input$rtp_series_type,
      region %in% input$rtp_state_select,
      gender == "Persons",
      year >= input$rtp_index_date) %>%
      arrange(date, region) %>%
      group_by(region) %>%
      mutate(index = 100*value/value[1]) 
    
    p = ggplot(df, 
               aes(x=date, 
                   y=index, 
                   colour = region)) + 
      geom_line() +
      labs(
        x=NULL,
        y= str_c("Index (Base:", month(min(df$date), abbr=F, label = T), year(min(df$date)), "=100)")
      )
    
    ggplotly(p)
    
  })
  
  output$demographic_plot <- renderPlotly({
    df = lf %>% 
      filter(type == input$dtp_indicator,
             series_type == "Trend",
             gender != "Persons") %>% 
      group_by(year, gender) %>% 
      summarise(v = mean(value)) %>% 
      ungroup() %>% 
      filter(year %in% c(max(year), input$dtp_baseline))
    p = ggplot(df, aes(x=gender, 
                       y=v, 
                       fill=as.factor(year),
                       text = paste0('Date: ',year, 
                                     '<br>', input$dtp_indicator, ': ',
                                     v))) + 
      geom_bar(stat='identity', position='dodge') +
      labs(
        x=NULL,
        y=input$dtp_indicator
      ) +
      theme(legend.position = 'none')
    
    ggplotly(p, tooltip='text')
  })
  
}

shinyApp(ui = ui, server = server)
