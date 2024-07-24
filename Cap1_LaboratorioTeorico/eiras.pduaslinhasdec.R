# eiras.pduaslinhasdec.R

source("eiras.dec.R")

# funcao p''
pdualinhasdec <- function(dA,dB)
{
  p <- seq(0,1,by=0.001)
  p2 <- rep(NA,length(p))
  for (i in 1:length(p))
  {
    p2[i] <- (p[i]*V(dA,p[i])) / (p[i]*V(dA,p[i]) + (1-p[i])*V(dB,1-p[i]))
  }

  plot(p,p2,
       main=paste0("Funcao recursiva de p''\ndA=",dA,", dB=",dB),
       xlim=c(0,1.08), ylim=c(0,1), 
       xlab="p",ylab="p''",type="l")
  # bissetriz
  curve(x^1,0,1,lty=2,add=TRUE)
  # legenda
  legend ("topleft",	
          c("p''=pV(dA,p)/(pV(dA,p)+(1-p)V(dB,p))",
            "p'' = p"),
          lwd=1,	
          lty=c(1,2),	
          cex=0.7,
          bty="n",
          bg="transparent")	
  
  # acha os pontos de equilibro, onde p=p''
  for (p in seq(0,1,by=0.0005))
  {
    p2 <- p*V(dA,p)/(p*V(dA,p)+(1-p)*V(dB,1-p))
    if(is.finite(p2))
    {
      if (round(p2,4)==round(p,4))# exemplo
      {
        points(p,p2,pch=21,col="#aaaaaa",bg="#aaaaaa",cex=0.8)
        text(p,round(p2,3),paste0("p = ",round(p,4)),cex=0.7,pos=4)
      }
    }
  }
}
