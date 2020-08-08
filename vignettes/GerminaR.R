## -----------------------------------------------------------------------------
library(GerminaR)
library(dplyr)
library(GerminaR)
dt <- prosopis %>% mutate(across(c(nacl, temp, rep), as.factor))

## ---- eval=FALSE--------------------------------------------------------------
#  GerminaQuant()

## -----------------------------------------------------------------------------
smr <- ger_summary(SeedN = "seeds"
                   , evalName = "D"
                   , data = dt
                   )

knitr::kable(head(smr, 10),align = "c")

## -----------------------------------------------------------------------------

grt <- ger_intime(Factor = "nacl"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = dt)

fplot(data = grt
      , type = "line"
      , x = "evaluation"
      , y = "mean"
      , groups = "nacl"
      , ylab = "Germination ('%')"
      , xlab = "days"
      , glab  = "NaCl (mM)"
      , legend = "top"
      , sig = NULL
      , color = T
      )


