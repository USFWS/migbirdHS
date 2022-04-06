library(hexSticker)
library(usmap)
library(ggplot2)
library(dplyr)
library(png)
library(patchwork)
library(cowplot)

sysfonts::font_add_google("Lato", "pf", regular.wt = 700)

mall <- readPNG("H:/hex/mall_silhouette_sm.png", native = TRUE)

usa49 <- 
  plot_usmap(
    data = statepop, 
    values = "pop_2015", 
    fill = "#FFFFFF", color = "#0270bb", exclude = "Hawaii") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 

orange_mall <-
  ggdraw(usa49) +
  draw_image(mall, x = 0, y = .1, width = .95, height = .9)

MBHS_hex <- hexSticker::sticker(
  orange_mall, 
  package = "migbirdMBHS", p_color = "#FFFFFF",
  p_size = 18, s_x = 1, s_y = .75, s_width = 1.2, s_height = .9,
  h_fill = "#faa225", h_color = "#ffd151", 
  filename = "man/figures/logo.png")
print(MBHS_hex)

magick::image_read("man/figures/logo.png")

#rstudioapi::restartSession()