download_graph_ui <- function(id) {
  fluidRow(
    column(width = 6,
           textInput(NS(id, "filename"),
                     "Filename",
                     placeholder = "Type a filename")
    ),
    column(width = 6,
           radioButtons(NS(id, "filetype"),
                        "File extension",
                        choices = c("png",
                                    "jpeg")
           )
    ),
    column(width = 6,
           numericInput(NS(id, "width"),
                        "Plot width (in pixels)",
                        value = 1000,
                        min = 800
           )
    ),
    column(width = 6,
           numericInput(NS(id, "height"),
                        "Plot height (in pixels)",
                        value = 500,
                        min = 300
           )
    ),
    column(width = 12,
           downloadButton(NS(id, "download_plot"), "Download chart", class = "download-button"),
           downloadButton(NS(id, "download_data"), "Download chart data", class = "download-button")
    )
  )
}