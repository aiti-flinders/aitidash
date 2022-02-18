# iviTreeUI <- function(id, data) {
#   
#   ns <- NS(id)
#   
# 
#   tabPanel("Occupation Demand",
#            plotlyOutput(ns("plot"), width = "100%", height = "600px"),
#            fluidRow(
#              box(width = 6, status= 'info',solidHeader = FALSE,
#                  sliderTextInput(
#                    inputId = ns('date'), 
#                    label = "Select Date",
#                    grid = TRUE,
#                    choices = zoo::as.yearmon(unique(data$date)),
#                    selected = zoo::as.yearmon(max(data$date)),
#                    animate = TRUE
#                  )
#              )
#            )
#   )
# }
# 
# iviTreeServer <- function(id, data,region) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       
#       create_data <- reactive({
#         df <- data %>%
#           filter(state == region(),
#                  date == as.Date(as.yearmon(input$date)),
#                  nchar(anzsco_2) == 2) %>%
#           mutate(value_share = 100*value/sum(value)) %>% 
#           # mutate(across(where(is.character), ~str_to_title(.x)),
#           #        occupation = ifelse(str_detect(occupation, "Total"),
#           #                            trimws(str_split(occupation, pattern = "[(]", simplify = TRUE)),
#           #                            occupation),
#           #        parents = ifelse(occupation == occupation_group, "", occupation_group)) %>%
#           # filter(occupation != "Total") %>%
#           top_n(n = 10, wt = value)
#     
#       })
#       
#       create_plot <- reactive({
#         
#         # ggplot(create_data(),
#         #             aes(area = vacancies,
#         #                 fill = occupation_group,
#         #                 label = str_c(occupation, as_comma(vacancies), sep = "\n"))) + 
#         #   geom_treemap() +
#         #   geom_treemap_text(colour = "white") +
#         #   labs(
#         #     title = "Top ten occupations by number of internet vacancies",
#         #     subtitle = str_c("advertised end of: ", input$date)
#         #   ) + 
#         #   theme_aiti(legend = "bottom", base_family = "Roboto") +
#         #   scale_fill_manual(values = c(aiti_blue, aiti_darkblue, aiti_grey, aiti_yellow, aiti_lightblue, aiti_greyblue))
#         plot_ly(create_data(), 
#                 labels = ~occupation,
#                 customdata = ~occupation_group,
#                 hovertext = ~value_share,
#                 parents = NA, 
#                 values = ~value,
#                 type = "treemap",
#                 tiling = list(packing = "binary"), 
#                 textfont = list(
#                   size = 20,
#                   family = "Roboto"),
#                 marker = list(
#                   line = list(color = aiti_darkblue),
#                   pad = list(t = 0, l = 0, r = 0, b = 0),
#                   colors = c("#001155", 
#                              "#001C68", 
#                              "#00287B", 
#                              "#00338E", 
#                              "#003EA1", 
#                              "#004AB3",
#                              "#0055C6",
#                              "#0060D9",
#                              "#006CEC",
#                              "#0077FF")),
#                 hovertemplate = paste0(
#                   "<b>%{customdata}</b><br>",
#                   "%{label}</br>",
#                   "Vacancies: %{value:,.0f}</br>", 
#                   "Share of all vacancies: %{hovertext:1f}%", 
#                   "<extra></extra>")) %>%
#           layout(title = list(
#             text = paste0("Top 10 Demanded Occupations - ", region(), " (",input$date, ")"),
#             font = list(
#               family = "Roboto"
#               )
#             ))
#         
#         
#         
# 
#       })
#       
#       output$plot <- renderPlotly({
#         create_plot()
#       })
#       
#     }
#   )
# }