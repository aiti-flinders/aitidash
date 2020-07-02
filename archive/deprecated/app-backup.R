library(shinydashboard)
library(tidyverse)
library(getabs)
library(lubridate)
library(plotly)
library(scales)
theme_set(theme_bw())

#### Data Setup ####
# Labour Force
# Read in Labour Force Data - Table 12 from ABS
labour_force_series = read_abs_local(filenames = "6202012.xls")
# Wrangle ABS data to a form which will be useful
lf = labour_force_series %>%
  separate(
    series,
    into = c("type", "gender", "region"),
    sep = ';',
    extra = 'drop'
  )
lf$type = str_remove_all(lf$type, "> ") %>% str_to_title()
lf$gender = str_remove_all(lf$gender, "> ")
lf$region = str_remove_all(lf$region, "> ")
lf = lf %>%
  mutate_at(vars("type", "gender", "region"), trimws) %>%
  mutate(year = year(date),
         month=month(date, label = T, abbr = F),
         day = day(date),
         month_year = paste(month, year, sep=" "))

#This Time Last Year/Month
TTL = function(series = NULL, state = "South Australia", g = "Persons", s_type = "Trend", last = "Year") 
{
  lf = lf %>% 
    filter(type == series,
           region == state,
           gender == g,
           series_type == s_type
    ) %>% 
    group_by(year,month, unit) %>%
    summarise(v=mean(value)) %>% 
    ungroup() %>%
    mutate(latest_month = lf %>%
             filter(year == max(year), month==max(month)) %>%
             .$month %>%
             .[1])
  if(last == "Year"){
    lf = lf %>%
    filter(month == latest_month)
  }
  else {
    lf = lf %>%
      filter(year == max(year)) 
      

  }
  lf = lf %>% 
    select(Year = year, Month=month, Value=v, unit=unit) %>%
    slice(n()-1)
  if(lf$unit[1] == "Percent"){lf = paste0(format(lf$Value, digits = 3),"%")}
  else{lf = format(1000*lf$Value, digits=6, big.mark=',')}
  return(lf)
}

YearChange = function(series = NULL, r = "South Australia", s_type = "Trend", change = 'year', from = max(lf$month)) 
{
  df = lf %>% filter(
    type == series,
    region == r,
    gender == "Persons",
    series_type == s_type
  ) %>%
    group_by(year, month) %>%
    summarise(change_average=mean(value)) %>%
    ungroup() %>%
    filter(month == from)
  A = df %>% slice(n()) %>% .$change_average
  B = df %>% slice(n()-1) %>% .$change_average
  x = c(100*(A-B)/B, A-B)
  return(x)
}

LatestValue = function(series = NULL, state = "South Australia",  g = "Persons",   s_type = "Trend",  d = max(lf$date))
{
  df = lf %>%
    filter(type == series,
           region == state,
           gender == g,
           series_type == s_type,
           date == d)
  if(df$unit[1] == "Percent"){df = paste0(format(df$value,digits=3), "%")}
  else{df = paste0(format(1000*df$value,digits=6, big.mark=","))}
  return(df)
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

#### Time Series Tab Panel ####
time_series_tab_panel_labour_market = tabPanel(title = "South Australia", plotlyOutput("time_series_plot", width='100%'),
                                 fluidRow(
                                   box(status ='info',solidHeader = TRUE,
                                     selectInput(
                                       inputId = "tstp_indicator",
                                       label = "Select Labour Market Indicator", 
                                       choices = c("Employed Total", "Unemployment Rate", "Employment To Population Ratio"),
                                       selected = "Unemployment Rate"),
                                     selectInput(
                                       inputId = 'tstp_series_type', 
                                       label = "Select Series Type", 
                                       choices = sort(unique(lf$series_type)), 
                                       selected = "Original"),
                                     sliderInput(
                                       inputId = 'tstp_date_range',
                                       label = "Select Date Range",
                                       min = min(lf$date),
                                       max = max(lf$date),
                                       value = c(min(lf$date), max(lf$date))
                                     )),
                                   box(status='info',solidHeader = TRUE,
                                     tableOutput('time_series_table')),
                                   box(status='info', solidHeader = TRUE,
                                     uiOutput('time_series_analysis'))
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
                                  selectInput(
                                    inputId = 'rtp_baseline', 
                                    label = "Select Baseline", 
                                    choices = unique(lf$month_year)[(length(unique(lf$month_year))-12):(length(unique(lf$month_year)))],
                                    selected = "December 2018")),
                                box(status='info',solidHeader=TRUE,
                                  uiOutput('')
                                )
                              )
)

#### Demographic Tab Panel ####
demographic_tab_panel_labour_market = tabPanel(title = "Demographic", plotlyOutput("demographic_plot", width='100%'),
                                               fluidRow(
                                                 box(status='info', solidHeader=TRUE,
                                                     selectInput(
                                                       inputId = 'dtp_indicator',
                                                       label = "Select Labour Market Indicator",
                                                       choices = c("Unemployment Rate", "Employed Total", "Employment To Population Ratio"),
                                                       selected = "Unemployment Rate"),
                                                     selectInput(
                                                       inputId = 'dtp_baseline',
                                                       label = "Select Comparison Year",
                                                       choices = c(2017, 2016),
                                                       selected = 2016)
                                                     )
                                                 )
)
#### Labour Market Tab ####
labour_market_tab = tabItem(tabName = 'labour_market', 
                            fluidRow(
                              infoBoxOutput("unemployment_box", width = 4),
                              infoBoxOutput("employment_box", width=4),
                              infoBoxOutput("employment_population_box", width=4)
                            ),
                            fluidRow(
                              tabBox(width = 12,
                                     time_series_tab_panel_labour_market,
                                     regional_tab_panel_labour_market,
                                     demographic_tab_panel_labour_market)
                            )
)


consumer_activity_tab = tabItem(tabName = 'consumer_activity',
                                fluidRow(
                                  infoBoxOutput("retail_trade_box", width=4)
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


                                       


#### UI ####
ui <- dashboardPage(
  header,
  sidebar,
  dashboardBody(
    tabItems(
    labour_market_tab,
    consumer_activity_tab,
    demographics_tab,
    economic_output_tab
    )
  )
)

#### Server ####
server <- function(input, output, session) {
  choices <- reactive({
    lf <- lf %>% filter(type == input$rtp_indicator)
    lf <- sort(unique(lf$series_type))
  })
  
  # unemploymentRate <- reactive({
  #   lf <- lf %>% filter(
  #     type == "Unemployment Rate",
  #     region == "South Australia",
  #     series_type == "Trend",
  #     gender == "Persons",
  #     date == max(date)
  #   )
  # })
  
#  observeEvent(input$rtp_indicator, 
#               {updateSelectInput(session = session, inputId = 'rtp_series_type', choices=choices())})
  
  output$time_series_table <- renderTable({
    lf %>% 
      filter(type == input$tstp_indicator,
             region == "South Australia",
             gender == "Persons",
             series_type == input$tstp_series_type
      ) %>% 
      group_by(year,month) %>%
      summarise(v=mean(value)) %>% 
      ungroup() %>%
      mutate(latest_month = lf %>%
               filter(year == max(year), month==max(month)) %>%
               .$month %>%
               .[1]) %>%
      filter(month == latest_month) %>% 
      select(Year = year, Month = month, Value = v) %>%
      filter(Year %in% c(max(Year), max(Year)-1)) %>%
      spread(key=Year, value=Value)},
    width = '100%',
    align = 'c'
    
  )
  
  output$time_series_analysis <- renderUI({HTML(
    paste("In",
          max(lf$month), 
          max(lf$year), 
          "South Australia's", 
          str_to_lower(input$tstp_series_type), 
          str_to_lower(input$tstp_indicator),
          "was",
          LatestValue(input$tstp_indicator,"South Australia", "Persons", input$tstp_series_type, max(lf$date)),
          ifelse(LatestValue(input$tstp_indicator,"South Australia", "Persons", input$tstp_series_type, max(lf$date)) > TTL(input$tstp_indicator, "South Australia", "Persons", input$tstp_series_type, last='Year'),
                 "up",
                 "down"),
          "from ",
          TTL(input$tstp_indicator, "South Australia", "Persons", input$tstp_series_type, last='Year'),
          "this time last year, and ",
          ifelse(LatestValue(input$tstp_indicator,"South Australia", "Persons", input$tstp_series_type, max(lf$date)) > TTL(input$tstp_indicator, "South Australia", "Persons", input$tstp_series_type, last='Month'),
                 "up",
                 "down"),
          "from",
          TTL(input$tstp_indicator, "South Australia", "Persons", input$tstp_series_type, last='Month'),
          "last month.<br><br>",
          "The unemployment rate in Australia was",
          LatestValue(input$tstp_indicator,"Australia", "Persons", input$tstp_series_type, max(lf$date)),
          "in",
          max(lf$month),
          ifelse(LatestValue(input$tstp_indicator,"Australia", "Persons", input$tstp_series_type, max(lf$date)) > TTL(input$tstp_indicator, "Australia", "Persons", input$tstp_series_type, last='Year'),
                 "up",
                 "down"),
          "from",
          TTL(input$tstp_indicator, "Australia", "Persons", input$tstp_series_type, last='Year'),
          "last year.", sep=" "))
    
  })
  
  output$unemployment_box <- renderInfoBox({
    infoBox(
      "Unemployment Rate: ",
      LatestValue("Unemployment Rate"),
      icon = icon(
        ifelse(
          YearChange('Unemployment Rate')[2] < 0,
          'arrow-alt-circle-down',
          'arrow-alt-circle-up'
        )
      ),
      fill = T,
      subtitle = paste(
        format(YearChange('Unemployment Rate')[2], digits=2),
        "% YR/YR Change"
      ),
      color = ifelse(
        YearChange('Unemployment Rate')[2] < 0,
        "green",
        "red"
      ),
      width = 4
    )
  })
  
  output$employment_box <- renderInfoBox({
    infoBox("Total Employment: " ,
            LatestValue("Employed Total"),
            icon = icon(
              ifelse(
                YearChange('Employed Total')[1] > 0,
                'arrow-alt-circle-up',
                'arrow-alt-circle-down'
              )
            ),
            fill = T,
            subtitle = paste0(
              format(YearChange('Employed Total')[1],digits = 2),
              "% YR/YR Change",
              sep=" "
            ),
            
            color = ifelse(
              YearChange('Employed Total')[1] > 0,
              "green",
              "red"
            ),
            width = 4)
  })
  
  output$employment_population_box <- renderInfoBox({
    infoBox("Employment to Population Ratio: ",
            LatestValue("Employment To Population Ratio"),
            icon = icon(
              ifelse(
                YearChange('Employment To Population Ratio')[1] > 0,
                'arrow-alt-circle-up',
                'arrow-alt-circle-down'
              )
            ),
            fill = T,
            subtitle = paste0(
              format(YearChange('Employment To Population Ratio')[1],digits = 2),
              "% YR/YR Change",
              sep=" "
            ),
            
            color = ifelse(
              YearChange('Employment To Population Ratio')[1] > 0,
              "green",
              "red"
            ),
            width = 4)
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
  
  output$time_series_plot <- renderPlotly({
    df = lf %>% filter(
      type == input$tstp_indicator,
      gender == "Persons",
      region == "South Australia",
      series_type == input$tstp_series_type,
      date >= input$tstp_date_range[1],
      date <= input$tstp_date_range[2]
    )
    
    p = ggplot(df, aes(x = date, 
                       y = value,
                       colour = gender, 
                       text = paste0('Date: ', format(date, "%Y-%b"),
                                    '<br>',input$tstp_indicator,': ', 
                                    if(df$unit[1]=="Percent"){paste0(format(value,digits=3), "%")}
                                    else{paste0(format(value*1000,big.mark=",", digits=6))}),
                       group = 1)) +
      geom_line() +
      labs(
        x = NULL,
        y=input$tstp_indicator,
        #title = input$tstp_indicator,
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
      else{scale_y_continuous(label = comma_format(scale = 1000))}
    ggplotly(p, tooltip='text')
    

    
  })
  output$region_plot <- renderPlotly({
    df = lf %>% filter(
      type == input$rtp_indicator,
      gender == "Persons",
      series_type == input$rtp_series_type
    ) %>%
      group_by(month_year, region) %>%
      summarise(change_average = mean(value),
                unit=first(unit)) %>%
      ungroup() %>%
      group_by(region) 
    if(df$unit[1] == "Percent"){
      df = df %>% 
        mutate(change = change_average-lag(change_average)) %>%
        filter(month_year == input$rtp_baseline)
    } else {
        df = df %>% 
          mutate(change = 100*(change_average-lag(change_average))/change_average) %>%
          filter(month_year == input$rtp_baseline)
      }
    p =  ggplot(df, aes(x=region, 
                 y= change, 
                 fill = region, 
                 text = paste0('Date: ', input$rtp_baseline,
                               '<br>',input$rtp_indicator,': ', 
                               paste0(format(change, digits = 2),"%")),
                 group = 1)) + 
      geom_bar(stat='identity') + 
      labs(
        x=NULL,
        y=paste(input$rtp_indicator, "Year over year % change", sep=' ')
      ) +
      scale_y_continuous(label = function(x) paste0(x,"%"))+
      scale_fill_manual(name = NULL,
                        values = c("#E69f00", "grey", "grey", "grey", "grey", "#56B4E9", "grey", "grey", "grey"),
                        breaks = c("Australia", "Australian Capital Territory", "New South Wales", "Northern Territory",
                                   "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia")) +
      scale_x_discrete(breaks = c("Australia", "Australian Capital Territory", "New South Wales", "Northern Territory",
                                  "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia"),
                       labels = c("AU", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")) +
      theme(legend.position="none")
    
    
    ggplotly(p, tooltip='text')
    
  })
  
  output$demographic_plot <- renderPlotly({
    p = lf %>% 
      filter(type == "Unemployment Rate",
             series_type == "Trend",
             gender != "Persons") %>% 
      group_by(year, gender) %>% 
      summarise(v = mean(value)) %>% 
      ungroup() %>% 
      filter(year %in% c(max(year), max(year)-1)) %>% 
      ggplot(aes(x=gender, y=v, fill=as.factor(year))) + 
      geom_bar(stat='identity', position='dodge')
    
    ggplotly(p)
  })
  
}

shinyApp(ui = ui, server = server)
