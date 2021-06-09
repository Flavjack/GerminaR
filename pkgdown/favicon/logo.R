library(hexSticker)
library(magick)
library(showtext)
library(dplyr)

# open https://fonts.google.com/
# font Paytone one

font_add_google(name = "Paytone One")
font_add_google(name = "Lobster")

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

file.copy(from = "pkgdown/favicon/img/GerminaR.png"
          , to = "man/figures/logo.png"
          , overwrite = T)

# -------------------------------------------------------------------------
# GerminaQuant ------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "germinaquant.png"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.81
        , s_width = 1.0
        , s_height = 1.0
        , package = "GerminaQuant"
        , p_family = "Lobster"
        , p_color = "#a64d79"
        , p_size = 21
        , p_y = 1.45
        , h_color = "#b0d744"
        , h_fill = NA
        , h_size = 1
        , url = "inkaverse.com"
        , u_color = "black"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.6
        , filename = "pkgdown/favicon/img/GerminaQuant.png")

