cp_theme <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          text=element_text(family="Arial", size=14),
          legend.text=element_text(size=14),
          legend.box.spacing = unit(0, "inches"),
          strip.background = element_rect(fill="white"))
}
