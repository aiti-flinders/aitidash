mytheme <- function(...) {
  create_theme(
    bs4dash_status(
      primary = aititheme::aiti_blue,
      success = "#64b478",
      danger = "#ffb24d",
      warning = "#fdd969",
    ),
    bs4dash_font(
      size_base = "1rem",
      family_base = "Roboto"
    ),
    bs4dash_color(
      blue = aititheme::aiti_blue,
      lightblue = aititheme::aiti_lightblue,
      navy = aititheme::aiti_darkblue,
      yellow = aititheme::aiti_yellow,
      gray_600 = aititheme::aiti_grey
    ),
    bs4dash_vars(
      card_title_font_size = "1.5rem",
      card_border_color = aititheme::aiti_darkblue
    ),
    bs4dash_sidebar_light(
      color = aititheme::aiti_darkblue,
      hover_color = aititheme::aiti_blue
    )
    #bs_vars(input_group_addon_bg = "red"),
    # bs_vars_button(
    #   default_color = "#FFF",
    #   default_bg = "#95a5a6",
    #   default_border = "#95a5a6"
    # )
    
    
  )
}
