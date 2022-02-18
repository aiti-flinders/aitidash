# iviUI <- function(id, data){
#   ns <- NS(id)
#   
#   occupation_groups <- data %>%
#     pull(occupation_group) %>%
#     unique() %>%
#     sort()
#   
#   occupation_choices <- data %>%
#     pull(occupation) %>%
#     unique() %>%
#     sort()
#   
#   tabPanel(title = uiOutput(ns("title_panel")),
#            plotlyOutput(ns("plot"), width = "100%"),
#            fluidRow(
#              box(status = "info", solidHeader = FALSE,
#                  selectInput(
#                    inputId = ns("occupation_group"),
#                    label = "Select Occupation Group",
#                    choices = occupation_groups
#                  )),
#              box(status = "info", solidHeader = FALSE,
#                  selectInput(
#                    inputId = ns("occupation"),
#                    label = "Select Occupation",
#                    choices = occupation_choices
#                  )),
#              fluidRow(
#                box(width = 12, status = "info", title = "Downloads", solidHeader = FALSE,
#                    downloadButton(
#                      outputId = ns("download_plot"),
#                      label = "Click here to download the chart as a .png",
#                      class = 'download-button'
#                    ),
#                    downloadButton(
#                      outputId = ns("download_data"),
#                      label = "Click here to download the chart data",
#                      class = 'download-button'
#                    ))
#              )
#            )
#   )
# }
# 
# ivi <- function(input, output, session, data, region) {
#   
#   output$title_panel <- renderText({
#     region()
#   })
#   
#   observeEvent(input$occupation_group,  {
#   updateSelectInput(session, "occupation", choices = data %>%
#       filter(occupation_group == input$occupation_group) %>%
#       pull(occupation) %>%
#       unique() %>%
#       sort()
#   )})
#   
#   create_data <- reactive({
#     df <- data %>%
#       filter(state == region(),
#              occupation == input$occupation)
#   })
#   
#   create_plot <- reactive({
#     
#     plot_title <- toupper(paste0(region(),": ", "Internet Vacancies (", input$occupation, ")"))
#     p <- ggplot(create_data(), aes(x = date, 
#                                    y = value,
#                                    text = paste0("Date: ", format(date, "%Y-%b"),
#                                                 "<br>", input$occupation, ": ", as_comma(value)),
#                                    group = occupation)) +
#       geom_line() +
#       labs(
#         x = NULL,
#         y = NULL,
#         title = plot_title
#       ) + 
#       scale_y_continuous(labels = comma_format()) + 
#       aiti_colour_manual(n = length(input$occupation)) +
#       theme_aiti(base_family = "Roboto")
#     
#     ggplotly(p, tooltip = "text") %>%
#       layout(autosize = TRUE,
#              legend = list(orientation = "h", 
#                            y = -0.15),
#              annotations = list(
#                x = 1,
#                y = -0.20,
#                text = "Source: AITI Economic Indicators",
#                showarrow = FALSE,
#                xref = "paper",
#                yref = "paper",
#                xanchor = "right", 
#                yanchor ="right"
#              ))
#     
#   })
#   
#   output$plot <- renderPlotly({
#     create_plot()
#   })
#   
#   output$download_plot <- downloadHandler(
#     filename = function(){
#       paste(input$indicator, "-plot.png", sep = '')
#     },
#     content = function(file) {
#       plotly_IMAGE(create_plot(), out_file = file)
#     }
#   )
#   
#   output$download_data <- downloadHandler(
#     filename = function() {
#       paste(input$indicator, "-data.csv", sep = '')
#     },
#     content = function(file) {
#       write.csv(create_data(), file, row.names = FALSE)
#     }
#   )
# }