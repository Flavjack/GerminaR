library(tidyverse)
library(hexSticker)
library(magick)
library(showtext)
library(cowplot)

# open https://fonts.google.com/
# font Paytone one

font_add_google(name = "Paytone One")
font_add_google(name = "Lobster")

showtext_auto()

# -------------------------------------------------------------------------
# GerminaR ----------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_germination.png"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 1.0
        , s_width = 2.1
        , s_height = 2.1
        , package = "GerminaR"
        , p_family = "Paytone One"
        , p_color = "#a64d79"
        , p_size = 25
        , p_y = 1.42
        , h_color = "transparent"
        , h_fill = "#b0d744"
        , h_size = 1
        , url = "inkaverse.com"
        , u_color = "white"
        , u_size = 8
        , u_angle = 30
        , u_x = 1.25
        , u_y = 0.21
        , filename = "pkgdown/favicon/img/GerminaR.png"
        )

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "GerminaR.png") %>%
  image_read()  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/GerminaR.png")

file.copy(from = "pkgdown/favicon/img/GerminaR.png"
          , to = "man/figures/logo.png"
          , overwrite = T)

# -------------------------------------------------------------------------
# GerminaQuant ------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_germinaquant.png"
                   ) %>% 
  image_read()

img <- logo %>% 
  image_scale("300") %>% 
  grid::rasterGrob()

plot <- ggdraw(img, ylim = c(0, 1.2)) + 
  draw_label("GerminaQuant"
             , colour = "#a64d79"
             , size = 140
             , fontfamily = "Lobster"
             , y = 1.1
             )

plot %>% 
  save_plot(filename = "pkgdown/favicon/img/germinaquant.png"
            , plot = .
            , base_asp = 1
            )

