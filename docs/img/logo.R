# -------------------------------------------------------------------------
# GerminaR ----------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- c("Paytone One", "Permanent Marker")

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#b1d842"
) %>% 
  include_image(value = "https://germinar.inkaverse.com/img/seed_germination.png"
                , size = c(5.5, 5.5)
                , position = c(2.55, 1.26)
                , opts = 'image_transparent("white")*image_modulate(brightness = 0)'
  ) %>%
  include_shape(size = 5.08
                , border_width = 0
                , border_color = NA
                , position = c(2.54, 2.54)
                , panel_color = "blue"
  ) %>%
  include_text(value = "GerminaR"
               , font[1]
               , size = 23
               , position = c(2.54, 3.55)
               , color = "#a64d79"
  ) %>%
  include_text(value = "inkaverse.com"
               , font[2]
               , size = 6
               , position = c(3.9, 0.96)
               , angle = 30
               , color = "white"
  )

label %>% label_print()

logo <- label %>%
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('blue') %>% 
  image_write("man/figures/logo.png")

# -------------------------------------------------------------------------
# GerminaQuant ------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- c("Lobster")

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0.5
                      , background = "white"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_germinaquant.png"
                , size = c(4.3, 4.3)
                , position = c(2.55, 2.18)
                ) %>%
  include_text(value = "GerminaQuant"
               , font = "Lobster"
               , size = 24
               , position = c(2.53, 4.69)
               , color = "#a64d79"
               ) 

label %>% label_print()

logo <- label %>%
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_write("pkgdown/favicon/img/GerminaQuant.png")

