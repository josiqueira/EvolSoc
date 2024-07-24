# eiras.pduaslinhasfncV.R

source("eiras.Vmax.R")

# solve p*(1-p)(exp(-a*p)-exp(-b(1-p)))/(p*exp(-a*p)+(1-p)exp(-b(1-p)))=0 , a>0, b>0, 1>=p>=0
# 
# exp(-a*p)
# ((a*(exp(-p)-exp(-1)))/(1-exp(-1)))
# 
# exp(-b(1-p))
# ((b*(exp(-(1-p))-exp(-1)))/(1-exp(-1)))
# 
# solve p*(1-p)(((a*(exp(-p)-exp(-1)))/(1-exp(-1)))-((b*(exp(-(1-p))-exp(-1)))/(1-exp(-1))))/(p*((a*(exp(-p)-exp(-1)))/(1-exp(-1)))+(1-p)((b*(exp(-(1-p))-exp(-1)))/(1-exp(-1))))=0 , a>0, b>0, 1>=p>=0

# funcao p''
pdualinhasfncV <- function(VAmax,VBmax)
{
  p <- seq(0,1,by=0.001)
  p2 <- rep(NA,length(p))
  for (i in 1:length(p))
  {
    p2[i] <- (p[i]*V(VAmax,p[i])) / (p[i]*V(VAmax,p[i]) + (1-p[i])*V(VBmax,1-p[i]))
  }

  plot(p,p2,
       main=paste0("Funcao recursiva de p''\nVAmax=",VAmax,", VBmax=",VBmax),
       xlim=c(0,1), ylim=c(0,1), 
       xlab="p",ylab="p''",type="l")
  # bissetriz
  curve(x^1,0,1,lty=2,add=TRUE)
  # legenda
  legend ("topleft",	
          c("p''=pV(VAmax,p)/(pV(VAmax,p)+(1-p)V(VBmax,p))",
            "p'' = p"),
          lwd=1,	
          lty=c(1,2),	
          cex=0.7,
          bty="n",
          bg="transparent")	
  
  # acha os pontos de equilibro, onde p=p''
  for (p in seq(0,1,length.out = 5e4))
  {
    p2 <- p*V(VAmax,p)/(p*V(VAmax,p)+(1-p)*V(VBmax,1-p))
    if(is.finite(p2))
    {
      if (round(p2,4)==round(p,4))# exemplo
        # VA <- 0.8
        # VB <- 0.5
        # pdualinhas_eq(VA,VB)
      {
        points(p,p2,pch=21,col="#aaaaaa",bg="#aaaaaa",cex=0.8)
        text(p,p2,paste0("p = ",round(p,4)),cex=0.7,pos=4)
      }
    }
  }
}

