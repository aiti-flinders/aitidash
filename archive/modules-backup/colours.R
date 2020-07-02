#AITI Colours
aiti_colours <- c(
  `dark blue` = "#001155",  
  `blue` = "#0077ff",
  `orange` = "#EB9486",
  `prussian blue` = "#272838",
  `yellow` = "#ffee55",
  `grey` = "#a1a1a1",
  `aqua` = "#c0d4ea",
  `light blue` = "#63acff"
)

aiti_cols <- function(...) {
  cols <- c(...)
  if(is.null(cols))
    return(aiti_colours)
  aiti_colours[cols]
}

aiti_palettes <- list(
  `main` = aiti_cols("dark blue", "blue", "orange", "grey"),
  `blues` = aiti_cols("blue", "dark blue", "prussian blue", "aqua", "light blue")
)

aiti_pal <- function(palette = "main", reverse = F, ...) {
  pal <- aiti_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_colour_aiti <- function(palette = "main", discrete = T, reverse = F, ...) {
  pal <- aiti_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("aiti_", palette), palette =
                     pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_aiti <- function(palette = 'main', discrete = T, reverse = F, ...) {
  pal <- aiti_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("aiti_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


