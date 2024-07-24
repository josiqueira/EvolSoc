# eiras.demodeltapdec.R

source("eiras.deltapdec.R")

dA <- 0.5
zA <- 1
dB <- 0.8
zB <- 1
p <- seq(0,1,by=0.0005)
deltap <- deltapdec(p,dA,dB,zA,zB)
minimo <- abs(round(deltap,4))
idxmin <- which(minimo==round(min(minimo,na.rm=TRUE),4))
points(p[idxmin],deltap[idxmin],pch="x")
text(p[idxmin],deltap[idxmin],paste0("p = ",round(p[idxmin],4)),cex=0.7,pos=4)
