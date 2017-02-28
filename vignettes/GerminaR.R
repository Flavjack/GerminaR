## ------------------------------------------------------------------------
library(GerminaR)
dim(GerminaR)
str(GerminaR)

## ---- eval=FALSE---------------------------------------------------------
#  GerminaQuant()

## ------------------------------------------------------------------------
dt <- GerminaR
smr <- ger_summary(SeedN = "NSeeds", evalName = "Ev", data = dt)
knitr::kable(head(smr, 10),align = "c")

## ------------------------------------------------------------------------
dt <- GerminaR
grs <- ger_GRS(evalName = "Ev", data = dt)
grs

## ------------------------------------------------------------------------
dt <- GerminaR
grp <- ger_GRP(SeedN = "NSeeds",evalName = "Ev", data = dt)
grp

## ------------------------------------------------------------------------
dt <- GerminaR
gas <- ger_ASG(SeedN = "NSeeds", evalName = "Ev", data = dt)
gas

## ------------------------------------------------------------------------
dt <- GerminaR
mgt <- ger_MGT(evalName = "Ev", data = dt)
mgt

## ------------------------------------------------------------------------
dt <- GerminaR
mgr <- ger_MGR(evalName = "Ev", data = dt)
mgr

## ------------------------------------------------------------------------
dt <- GerminaR
gsp <- ger_GSP(evalName = "Ev", data = dt)
gsp

## ------------------------------------------------------------------------
dt <- GerminaR
syn <- ger_SYN(evalName = "Ev", data = dt)
syn

## ------------------------------------------------------------------------
dt <- GerminaR
unc <- ger_UNC(evalName = "Ev", data = dt)
unc

## ------------------------------------------------------------------------
dt <- GerminaR
sdg <- ger_SDG(evalName = "Ev", data = dt)
sdg

## ------------------------------------------------------------------------
dt <- GerminaR
cvg <- ger_CVG(evalName = "Ev", data = dt)
cvg

## ------------------------------------------------------------------------
dt <- GerminaR
vgt <- ger_VGT(evalName = "Ev", data = dt)
vgt

## ------------------------------------------------------------------------
dt <- GerminaR
grt <- ger_intime(Factor = "Salt", SeedN = "NSeeds", evalName = "Ev", method = "percentage", data = GerminaR)

