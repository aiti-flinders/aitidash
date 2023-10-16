mytheme <- function(...) {
  create_theme(
    bs4dash_status(
      primary = get_colour("Legacy Blue"),
      success = "#64b478",
      danger = "#ffb24d",
      warning = "#fdd969",
    ),
    bs4dash_font(
      size_base = "1rem",
      family_base = "Roboto"
    ),
    bs4dash_color(
      blue = get_colour("Legacy Blue"),
      lightblue = get_colour("Legacy Blue Highlight"),
      navy = get_colour("Concrete Grey"),
      yellow = get_colour("Flinders Gold"),
      gray_600 = get_colour("Soft Black")
    ),
    bs4dash_vars(
      card_title_font_size = "1.5rem",
      card_border_color = get_colour("Concrete Grey")
    ),
    bs4dash_sidebar_light(
      color = get_colour("Concrete Grey"),
      hover_color = get_colour("Legacy Blue")
    )
    #bs_vars(input_group_addon_bg = "red"),
    # bs_vars_button(
    #   default_color = "#FFF",
    #   default_bg = "#95a5a6",
    #   default_border = "#95a5a6"
    # )
    
    
  )
}
