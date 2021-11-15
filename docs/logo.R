# -------------------------------------------------------------------------
# GerminaR ----------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

#> https://github.com/terinjokes/StickersStandard

huito_fonts(c("Paytone One", "Lobster"))

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#b1d842"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_germination.png"
                , size = c(5, 5)
                , position = c(2.55, 2.52)
                ) %>%
  include_shape(size = 4.2
                , border_width = 3
                , border_color = NA
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_color = "#458dcb"
                , panel_size = 5.08
                ) %>%
  include_text(value = "GerminaR"
               , font = "Paytone One"
               , size = 23
               , position = c(2.54, 3.56)
               , color = "#a64d79"
               ) %>% 
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.9, 0.96)
               , angle = 30
               , color = "white"
               ) %>%
  label_print(filename = "pkgdown/favicon/img/GerminaR"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 250
              , mode = "s"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "GerminaR.pdf") %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('#458dcb') %>% 
  image_write("man/figures/logo.png")

# -------------------------------------------------------------------------
# GerminaQuant ------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

huito_fonts(c("Paytone One", "Lobster"))

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
  ) %>% 
  label_print(filename = "pkgdown/favicon/img/GerminaQuant"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 250
              , mode = "s"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "GerminaQuant.pdf") %>%
  image_read_pdf()  %>% 
  image_write("pkgdown/favicon/img/GerminaQuant.png")


