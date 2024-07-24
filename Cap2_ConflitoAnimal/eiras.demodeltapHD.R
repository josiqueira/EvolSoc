# eiras.demodeltapHD.R

source("eiras.deltapHD.R")

demodeltapHD <- function (w0,v,c)
{
  p <- seq(0,1,by=0.0005)
  deltap <- deltapHD(p, w0, v, c)
  minimo <- abs(round(deltap,4))
  idxmin <- which(minimo==round(min(minimo,na.rm=TRUE),4))
  idxmin <- c(idxmin[1],idxmin[round(length(idxmin)/2+1,0)],idxmin[length(idxmin)])
  points(p[idxmin],deltap[idxmin],pch="x")
  text(p[idxmin],deltap[idxmin],paste0("p = ",round(p[idxmin],4)),cex=0.7,pos=4)
}
