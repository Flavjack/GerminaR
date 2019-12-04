# -------------------------------------------------------------------------
# metamorphosis function test ---------------------------------------------
# -------------------------------------------------------------------------

# library(googlesheets4)
# library(tidyverse)
# 
# sheets_auth(T)
# url <- "https://docs.google.com/spreadsheets/d/1iIGsgXU_IBjmwqJ_Vo0sICUZpuTzZ_JGwD5ZgBG1jlk/edit#gid=1365339641"
# gs <- as_sheets_id(url)
# # browseURL(url)
# 
# # Data dictionary is the same for all experiments in quinoa ---------------
# 
# # Import dictionary -------------------------------------------------------
# 
# dc <- gs %>%
#   sheets_read(sheet = "var") 
# 
# # Case 01 -----------------------------------------------------------------
# 
# # Importa fieldbook -------------------------------------------------------
# 
# fb1 <- gs %>%
#   sheets_read(sheet = "fb_1")
# 
# 
# 
# mdf1 <- fb1 %>%
#   metamorphosis(fielbook = .,
#                 dictionary = dc,
#                 from = "fb1_org_name",
#                 to = "new_name",
#                 index = "type",
#                 colnames = c("colname", "var"))
# 
# nfb_1 <- mdf1$fielbook_new
# str(nfb_1)
# 
# # Case 02 -----------------------------------------------------------------
# 
# # Importa fieldbook and reshape field book---------------------------------
# 
# fb2 <- gs %>%
#   sheets_read(sheet = "fb_2") %>% 
#   gather(-parcela, -trat, key = "var", value = "val") %>% 
#   separate(var, into = c("var", "pheno", "sample"), sep = "_") %>% 
#   spread(var, val)
# 
# # Import dictionary -------------------------------------------------------
# 
# mdf2 <- fb2 %>%
#   metamorphosis(fielbook = .,
#                 dictionary = dc,
#                 from = "fb2_org_name",
#                 to = "new_name",
#                 index = "type",
#                 colnames = c("colname", "var"))
# 
# nfb_2 <- mdf2$fielbook_new
# nfb_2
# str(nfb_2)


# -------------------------------------------------------------------------
# web_table function test --------------------------------------------------
# -------------------------------------------------------------------------

# library(googlesheets4)
# library(tidyverse)
# 
# sheets_auth(T)
# url <- "https://docs.google.com/spreadsheets/d/1iIGsgXU_IBjmwqJ_Vo0sICUZpuTzZ_JGwD5ZgBG1jlk/edit#gid=1365339641"
# gs <- as_sheets_id(url)
# # browseURL(url)
# 
# dc <- gs %>%
#   sheets_read(sheet = "var") 
# 
# dc %>% web_table(buttons = c("excel"))

