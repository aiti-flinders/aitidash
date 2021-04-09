dashboard_box <- function(..., 
                          title, 
                          width = 4, 
                          status = "primary", 
                          solidHeader = FALSE, 
                          headerBorder = TRUE, 
                          collapsible = FALSE) {
  box(
    ..., 
    title = title, 
    width = width, 
    status = status, 
    solidHeader = solidHeader, 
    headerBorder = headerBorder, 
    collapsible = collapsible
  )
  
}