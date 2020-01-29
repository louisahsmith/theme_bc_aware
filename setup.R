# this was the original font I used, but apparently this will only work if
# you've ever had Office 2010, Windows 7, or Windows 8...
# font_to_use <- "Gabriola"
# so instead:
font_to_use <- "Brush Script MT"

# colors
darkpink <- "#B93476"
lighterpink <- "#F282BC"
lightpink <- "#fce6f1"
purps <- RColorBrewer::brewer.pal(7, "PuRd")

# ggplot theme
theme_bc_aware <- function() {
  darkpink <- "#B93476"
  lighterpink <- "#F282BC"
  lightpink <- "#fce6f1"
  theme_dark() %+replace%
    theme(
      title = element_text(color = darkpink, family = font_to_use, face = "italic", size = rel(1.5)),
      panel.grid.major = element_line(color = lighterpink),
      panel.grid.minor = element_line(linetype = "dashed", color = lighterpink),
      panel.background = element_rect(fill = lightpink),
      panel.border = element_rect(color = darkpink, fill = NA),
      axis.line = element_line(color = darkpink),
      axis.ticks = element_line(color = darkpink),
      axis.text = element_text(color = darkpink, family = font_to_use, face = "italic", size = rel(1.3)),
      strip.text = element_text(color = darkpink, family = font_to_use, face = "italic", size = rel(1.3)),
      strip.background = element_rect(color = "white"),
      legend.key = element_rect(fill = lightpink, color = NA),
      legend.text = element_text(color = darkpink, family = font_to_use, face = "italic", size = rel(1.3))
    )
}

# from http://clipart-library.com/breast-cancer-ribbon.html
logo <- magick::image_read(here::here("img", "pinktober.jpg"))
ribbon <- magick::image_read(here::here("img", "ribbon.png"))
