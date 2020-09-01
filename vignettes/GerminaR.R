## ----message=F, warning=FALSE-------------------------------------------------
library(GerminaR)
library(dplyr)
library(knitr)

dt <- prosopis %>% 
  mutate(across(c(nacl, temp, rep), as.factor))


## ---- eval=FALSE--------------------------------------------------------------
#  GerminaQuant()

## -----------------------------------------------------------------------------
smr <- ger_summary(SeedN = "seeds"
                   , evalName = "D"
                   , data = dt
                   )

knitr::kable(head(smr, 10),align = "c")

## ---- out.width="60%"---------------------------------------------------------
## Mean Germination Time (MGT)

# analysis of variance

av <- aov(formula = mgt ~ nacl*temp + rep, data = smr)

# mean comparison test

mc_mgt <- ger_testcomp(aov = av
                       , comp = c("temp", "nacl")
                       , type = "snk")

# data result

mc_mgt$table %>% 
   kable(caption = "Mean germination time comparison")


# bar graphics for mean germination time

plot <- mc_mgt$table %>% 
  fplot(data = .
       , type = "bar" 
       , x = "temp"
       , y = "mgt"
       , groups = "nacl"
       , limits = c(0,9)
       , brakes = 1
       , ylab = "Mean germination time (days)"
       , xlab = "Temperature (ºC)"
       , glab = "NaCl (MPa)"
       , legend = "top"
       , sig = "sig"
       , error = "ste"
       , color = T
       )

plot


## ---- out.width="60%"---------------------------------------------------------

grt <- ger_intime(Factor = "nacl"
                  , SeedN = "seeds"
                  , evalName = "D"
                  , method = "percentage"
                  , data = dt)

plot <- grt %>% 
  fplot(data = .
        , type = "line"
        , x = "evaluation"
        , y = "mean"
        , groups = "nacl"
        , ylab = "Germination ('%')"
        , xlab = "days"
        , glab  = "NaCl (mM)"
        , sig = NULL
        , color = T
        )

plot


