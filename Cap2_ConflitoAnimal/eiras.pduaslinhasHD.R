# eiras.pduaslinhasHD.R

source("eiras.HD.R")
source("eiras.deltapHD.R")

# funcao p''
pdualinhasHD <- function(w0, v, c)
{
  p <- seq(0,1,by=0.001)
  p2 <- rep(NA,length(p))
  for (i in 1:length(p))
  {
    p2[i] <- (p[i]*W("H", p[i], w0, v, c)) / (p[i]*W("H", p[i], w0, v, c) + (1-p[i])*W("D", p[i], w0, v, c))
  }

  plot(p,p2,
       main=paste0("Funcao recursiva de p''\nw0=",w0,", v=",v,", c=",c),
       xlim=c(0,1.08), ylim=c(0,1), 
       xlab="p",ylab="p''",type="l")
  # bissetriz
  curve(x^1,0,1,lty=2,add=TRUE)
  # legenda
  legend ("topleft",	
          c("p''=pW(H)/(pW(H)+(1-p)V(D))",
            "p'' = p"),
          lwd=1,	
          lty=c(1,2),	
          cex=0.7,
          bty="n",
          bg="transparent")	
  
  # acha os pontos de equilibro, onde p=p''
  deltap <- deltapHD(p, w0, v, c, graph=FALSE)
  minimo <- abs(round(deltap,4))
  idxmin <- which(minimo==round(min(minimo,na.rm=TRUE),4))
  idxmin <- c(idxmin[1],idxmin[round(length(idxmin)/2+1,0)],idxmin[length(idxmin)])
  p1 <- p[idxmin]
  p2 <- (p1*W("H", p1, w0, v, c)) / (p1*W("H", p1, w0, v, c) + (1-p1)*W("D", p1, w0, v, c))
  points(p1,p2,pch=21,col="#aaaaaa",bg="#aaaaaa",cex=0.8)
  text(p1,round(p2,3),paste0("p = ",round(p1,4)),cex=0.7,pos=4)
  
  dt <- data.frame(p1,p2)
  return(dt)
}
