## -----------------------------------------------------------------------------
library(GerminaR)
dim(prosopis)
str(prosopis)

## ---- eval=FALSE--------------------------------------------------------------
#  GerminaQuant()

## -----------------------------------------------------------------------------
dfr <- prosopis
smr <- ger_summary(SeedN = "seeds", evalName = "D", data = dfr)
knitr::kable(head(smr, 10),align = "c")

## -----------------------------------------------------------------------------
dfr <- prosopis
grs <- ger_GRS(evalName = "D", data = dfr)
grs

## -----------------------------------------------------------------------------
dfr <- prosopis
grp <- ger_GRP(SeedN = "seeds",evalName = "D", data = dfr)
grp

## -----------------------------------------------------------------------------
dfr <- prosopis
gas <- ger_ASG(SeedN = "seeds", evalName = "D", data = dfr)
gas

## -----------------------------------------------------------------------------
dfr <- prosopis
mgt <- ger_MGT(evalName = "D", data = dfr)
mgt

## -----------------------------------------------------------------------------
dfr <- prosopis
mgr <- ger_MGR(evalName = "D", data = dfr)
mgr

## -----------------------------------------------------------------------------
dfr <- prosopis
gsp <- ger_GSP(evalName = "D", data = dfr)
gsp

## -----------------------------------------------------------------------------
dfr <- prosopis
syn <- ger_SYN(evalName = "D", data = dfr)
syn

## -----------------------------------------------------------------------------
dfr <- prosopis
unc <- ger_UNC(evalName = "D", data = dfr)
unc

## -----------------------------------------------------------------------------
dfr <- prosopis
sdg <- ger_SDG(evalName = "D", data = dfr)
sdg

## -----------------------------------------------------------------------------
dfr <- prosopis
cvg <- ger_CVG(evalName = "D", data = dfr)
cvg

## -----------------------------------------------------------------------------
dfr <- prosopis
vgt <- ger_VGT(evalName = "D", data = dfr)
vgt

## -----------------------------------------------------------------------------
dfr <- prosopis
grt <- ger_intime(Factor = "nacl", SeedN = "seeds", evalName = "D", method = "percentage", data = dfr)
head(grt, 10)

## ----  warning=FALSE----------------------------------------------------------
dfr <- prosopis
grt <- ger_intime(Factor = "nacl", SeedN = "seeds", evalName = "D", method = "percentage", data = dfr)

fplot(data = grt, type = "line",
      x = "evaluation",
      y = "mean",
      z = "nacl",
      ylab = "Germination ('%')",
      xlab = "days", lgl = "NaCl (mM)",
      lgd = "top", color = F)


