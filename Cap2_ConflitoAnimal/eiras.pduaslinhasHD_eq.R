# eiras.pduaslinhasHD_eq.R

source("eiras.pduaslinhasHD.R")

# funcao p''
pdualinhasHD_eq <- function(w0, v, c, pini)
{
  dt_ptos <- pdualinhasHD(w0, v, c)
  points (dt_ptos, pch=21, col="black", bg="white")
  # ponto de partida
  for (p0 in pini)
  {
    p <- p0
    p2 <- (p*W("H", p, w0, v, c)) / (p*W("H", p, w0, v, c) + (1-p)*W("D", p, w0, v, c))
    points(p,0,pch="x",col="#666666")
    arrows(p,0,p,p2,lty=1,col="#666666",length=0.1,angle=15)
    start <- TRUE
    while(1)
    {
      p2 <- (p*W("H", p, w0, v, c)) / (p*W("H", p, w0, v, c) + (1-p)*W("D", p, w0, v, c))
      if (p2 == p)
      {
        points(p,p2,pch=21,col="black",bg="black",cex=0.8)
        break
      }
      if (!start)
      {
        lines(c(p,p),c(p,p2),lty=1,col="#666666")
        # arrows(p,p,p,p2,lty=1,length=0.1,angle=15,col="#666666")
      }
      lines(c(p,p2),c(p2,p2),lty=1,col="#666666")
      # arrows(p,p2,p2,p2,lty=1,length=0.1,angle=15,col="#666666")
      p <- p2
      start <- FALSE      
    }
  } # for v
}
