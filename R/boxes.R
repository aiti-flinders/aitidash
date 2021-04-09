box_map <- function(...) {
  data.frame(
    id = c("unemployment_rate",
           "unemployed",
           "employment_total",
           "participation_rate", 
           "employment_ft",
           "employment_pt",
           "underemployed",
           "underutilised",
           "underemployment_rate",
           "underutilisation_rate", 
           "hours_worked_total", 
           "labour_force_total"),
    indicator = c("Unemployment rate",
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
                  "Labour force total"),
    reverse = c(TRUE, 
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                FALSE,
                FALSE),
    percent = c(TRUE,
                FALSE,
                FALSE,
                TRUE,
                FALSE,
                FALSE,
                FALSE,
                FALSE,
                TRUE,
                TRUE,
                FALSE,
                FALSE),
    footer =  paste0("Data last updated: ")
  )
}


boxes_ui <- function(id, indicator = NULL, footer = NULL, plot = TRUE) {
  
  ns <- NS(id)
  bs4Card(width = 6,
          title = uiOutput(ns("box_title")),
          solidHeader = FALSE, 
          status = 'primary', 
          collapsible = F,
          footer = footer,
          uiOutput(ns("box_ui")),
          if(plot) {plotOutput(height = 60, ns("plot"))}
  )
}

boxes_ui_industry <- function(id) {
  ns <- NS(id)
  infoBoxOutput(ns('box'), width = 6)
}

#' Server side part of box creation
#'
#' @param id id of UI element
#' @param data dataframe
#' @param indicator box indicator
#' @param region box region
#' @param reverse reverses box colours. Default FALSE
#' @param percent formats box for an indicator which is a rate. Default FALSE

boxes_server <- function(id, data, indicator, region,  reverse = FALSE, percent = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      
      create_data <- reactive({
        rows <- data %>%
          filter(indicator == (!!indicator),
                 state == region()) %>%
          nrow()
        
        if(rows == 0) {
          box_text_current <- "Data Not Available"
          box_text_mom <- "Data Not Available"
          box_text_yoy <- "Data Not Available"
          box_subtitle <- paste("For", region)
          colour <- "warning"
          colour_y <- "warning"
          icon <- "ban"
          icon_y <- "ban"
          
        } else {
          cu <- current(data, list('indicator' = indicator, 'state' = region()), print = FALSE) 
          ly <- cu - last_value(data, list('indicator' = indicator, 'state' = region()), "year", print = FALSE)
          lm <- cu - last_value(data, list('indicator' = indicator, 'state' = region()), 'month', print = FALSE)
          
          
          if(percent == T) {
            if (length(ly) >= 1) {
              box_text_yoy <- paste0(ifelse(ly > 0, "+", ""), as_percentage_point(ly))
            } else {
              box_text_yoy <- "N/A"
            }
            if (length(lm) >= 1) {
              box_text_mom <- paste0(ifelse(lm > 0, "+", ""), as_percentage_point(lm))
            } else {
              box_text_mom <- "N/A"
            }
            box_text_current <- as_percent(cu)
            
          } else {
            if (length(ly) >=1) {
              box_text_yoy <- paste0(ifelse(ly > 0, "+", ""), as_comma(ly)) 
            } else {
              box_text_yoy <- "N/A"
            }
            if (length(lm) >= 1) {
              box_text_mom <- paste0(ifelse(lm > 0, "+", ""), as_comma(lm))
            } else {
              box_text_mom <- "N/A"
            }
            #box_text_current <- paste0(as_comma(cu), " (",box_text_mom,")")
            box_text_current <- as_comma(cu)
          }
          
          if (reverse == T) {
            colour <- ifelse(lm < 0, 'success', 'danger')
            colour_y <- ifelse(ly < 0, 'success', 'danger')
            icon <- ifelse(lm < 0, 'arrow-down', 'arrow-up')
            icon_y <- ifelse(ly < 0, 'arrow-down', 'arrow-up')
            
          } else { 
            colour <- ifelse(lm > 0, 'success', 'danger')
            colour_y <- ifelse(ly > 0, 'success', 'danger')
            icon <- ifelse(lm > 0, 'arrow-up', 'arrow-down')
            icon_y <- ifelse(ly > 0, 'arrow-up', 'arrow-down')
          }
          
          if (length(colour_y) == 0) {colour_y <- "warning"}
          if (length(icon_y) == 0) {icon_y <- "ban"}
          
          box_subtitle <- paste0("Change over year: ", box_text_yoy)
          
        }
        
        tibble(
          box_title =  paste0(boxes_names()[boxes_names()$indicator == indicator, ]$label, ": ", box_text_current),
          box_text_current,
          box_text_mom,
          colour,
          icon,
          box_text_yoy,
          colour_y,
          icon_y
        )

      })
      
      output$box_title <- renderUI({
        create_data()$box_title
      })
      
      output$plot <- renderPlot({
        validate(
          need(create_data()$box_text_current != "Data Not Available", message = FALSE)
        )
        
        if (indicator %in% labour_market_indicators()) {
        
        abs_plot(indicator = indicator,
                 years = 2018,
                 states = region(),
                 ages = "Total (age)",
                 sex = "Persons",
                 series_types =  "Seasonally Adjusted",
                 compare_aus  = FALSE, 
                 plotly = FALSE,
                 void = TRUE) 
        } else {
          jobkeeper_plots(indicator = indicator, 
                          states = region(),
                          compare_aus = FALSE,
                          plotly = FALSE,
                          void = TRUE)
        }
      })
      
      output$box_ui <- renderUI({
        fluidRow(
          column(
            width = 6,
            descriptionBlock(
              number = create_data()$box_text_mom,
              numberColor = create_data()$colour,
              numberIcon = icon(create_data()$icon),
              text = "change over month",
              #header = box_text_current
            )
          ),
          column(
            width = 6,
            descriptionBlock(
              rightBorder = FALSE, 
              number = create_data()$box_text_yoy,
              numberColor = create_data()$colour_y,
              numberIcon = icon(create_data()$icon_y),
              text = "change over year",
              #header = box_text_current
            )
          )
        )

        
      })
    }
  )
}



boxes_server_industry <- function(id, data, indicator, region, footer = NULL, growth = FALSE, percent = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
      
      data <- data[data$industry != "Total (industry)", ]
      
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
        
        if (nrow(data[data$indicator == indicator & data$state == region(), ]) == 0) {
          box_title <- indicator
          box_value <- "Data Not Available"
          box_text_current <- "Data Not Available"
          box_text_qoq <- "Data Not Available"
          box_text_yoy <- "Data Not Available"
          box_subtitle <- paste("For", region())
          colour <- "warning"
          colour_y <- "warning"
          icon <- "ban"
          icon_y <- "ban"
          industry_icon <- "ban"
        } else {
          
          box_title <- indicator
          ly <- last_value(data, list(indicator = indicator, state = region()), ym = "year", print = FALSE) %>%
            pull(value)
          
          lq <- last_value(data, list(indicator = indicator, state = region()), ym = "quarter", print = FALSE) %>%
            pull(value)
          
          cu <- current(data, list(indicator = indicator, state = region()), print = FALSE) %>%
            mutate(growth_yoy = value - ly,
                   growth_qoq = value - lq) %>%
            filter(value == max(value))
          
          box_text_current <- as_comma(cu$value)
          box_text_yoy <- paste0(ifelse(cu$growth_yoy > 0, "+", ""), as_comma(cu$growth_yoy))
          box_text_qoq <- paste0(ifelse(cu$growth_qoq > 0, "+", ""), as_comma(cu$growth_qoq))
          box_subtitle <- paste0(box_text_current, " (", box_text_yoy, ")")
          box_value <- cu$industry
          
          colour <- ifelse(cu$growth_qoq > 0, 'success', 'danger')
          colour_y <- ifelse(cu$growth_yoy > 0, 'success', 'danger')
          icon <- ifelse(cu$growth_qoq > 0, 'arrow-up', 'arrow-down')
          icon_y <- ifelse(cu$growth_yoy > 0, 'arrow-up', 'arrow-down')
          
          # colour <- case_when(
          #   cu$growth_qoq >0 & indicator != "Underemployed total" ~ "success",
          #   TRUE ~'danger'
          # )
          industry_icon <- icon_tib %>% 
            filter(industry ==  box_value) %>% 
            pull(icon)
        }
        
        # box <- infoBox(
        #   width = 6,
        #   fill = FALSE,
        #   title = box_title,
        #   value =  box_value,
        #   subtitle = box_subtitle,
        #   color = colour,
        #   icon = icon(icon)
        # )
        
        box <- bs4Card(
          width = 6,
          title = paste0(box_title, ": ", box_text_current),
          solidHeader = FALSE,
          status = "primary",
          collapsible = FALSE,
          footer = footer,
          fluidRow(
            column(
              width = 4,
              descriptionBlock(
                number = box_value,
                numberColor = colour,
                text = "largest industry"
              )
            ),
            column(
              width = 4,
              descriptionBlock(
                number = box_text_qoq,
                numberColor = colour,
                numberIcon = icon(icon),
                text = "change over quarter"
              )
            ),
            column(
              width = 4,
              descriptionBlock(
                rightBorder = FALSE,
                number = box_text_yoy,
                numberColor = colour_y,
                numberIcon = icon(icon_y),
                text = "change over year"
              )
            )
          ))
        
        return(box)
        
      })
    }
  )
}



