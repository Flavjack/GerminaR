url <- "https://docs.google.com/spreadsheets/d/1G9555qEa1TJ5NDJV4zGAffY-YZTJTE0keDpiANnJpIo/edit#gid=938961712"
dta <- gsheet2tbl(url)
attach(dta)

??germinar

dt <-  dta[2:17]
  
GRS(dt)
GRP(NumSeeds,dt)
ASG(NumSeeds,dt)
MGT(dt)
MGR(dt)
cvg(dt)
VGT(dt)
SDG(dt)
CoVG(dt)
GRU(dt)
GSI(dt)
