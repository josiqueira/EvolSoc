# eiras.demodeltapfncV.R

source("eiras.deltapfncV.R")

VA <- 0.8
zA <- 100
VB <- 0.5
zB <- 100
p <- seq(0,1,by=0.0001)
deltap <- deltapfncV(p,VA,VB,zA,zB)
minimo <- abs(deltap)
idxmin <- which(minimo==min(minimo,na.rm=TRUE))
points(p[idxmin],deltap[idxmin],pch="x")
text(p[idxmin],deltap[idxmin],paste0("p = ",p[idxmin]),cex=0.7,pos=4)
