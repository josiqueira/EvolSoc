# eiras.pduaslinhasfncV_eq.R

source("eiras.Vmax.R")

pdualinhasfncV_eq <- function(VAmax,VBmax)
{
  options(warn = -1)
  
  # caminho recursivo, iniciando com p baixo e p alto
  pini <- c(0.02,0.98)
  for (v in pini)
  {
    # funcao p''
    p <- seq(0,1,by=0.001)
    p2 <- rep(NA,length(p))
    for (i in 1:length(p))
    {
      p2[i] <- (p[i]*V(VAmax,p[i])) / (p[i]*V(VAmax,p[i]) + (1-p[i])*V(VBmax,1-p[i]))
    }
    plot(p,p2,
         main=paste0("Funcao recursiva de p''\nVAmax=",VAmax,", VBmax=",VBmax),
         xlim=c(0,1.08), ylim=c(0,1), 
         xlab="p",ylab="p''",type="l")
    # bissetriz
    curve(x^1,0,1,lty=2,add=TRUE)
    # legenda
    legend ("topleft",	
            c("p''=pV(VAmax,p)/(pV(VAmax,p)+(1-p)V(VBmax,p))",
              "p'' = p"),
            lwd=c(2,1),	
            lty=c(1,2),	
            cex=0.7,
            bty="n",
            bg="transparent")	
    
    # pontos de equilibro onde p=p''
    for (p in seq(0,1,length.out = 5e4))
    {
      p2 <- p*V(VAmax,p)/(p*V(VAmax,p)+(1-p)*V(VBmax,1-p))
      if(is.finite(p2))
      {
        if (round(p2,4)==round(p,4))# exemplo
        {
          points(p,p2,pch=21,col="black",bg="white",cex=0.8)
          text(p,p2,paste0("p = ",round(p,4)),cex=0.7,pos=4)
        }
      }
    }

    # caminho recursivo
      # ponto de partida
    p <- v
    p2 <- p*V(VAmax,p)/(p*V(VAmax,p)+(1-p)*V(VBmax,(1-p)))
    points(p,0,pch="x",col="#666666")
    arrows(p,0,p,p2,lty=1,col="#666666",length=0.1,angle=15)
    # lines(c(p,p),c(0,p2),lty=1,col="#666666")
    start <- TRUE
    while(1)
    {
      p2 <- p*V(VAmax,p)/(p*V(VAmax,p)+(1-p)*V(VBmax,(1-p)))
      if (p2 == p)
      {
        points(p,p2,pch=21,col="black",bg="black",cex=0.8)
        break
      }
      if (!start)
      {
        lines(c(p,p),c(p,p2),lty=1,length=0.1,angle=15,col="#666666")
        # arrows(p,p,p,p2,lty=1,length=0.1,angle=15,col="#666666")
      }
      lines(c(p,p2),c(p2,p2),lty=1,length=0.1,angle=15,col="#666666")
      # arrows(p,p2,p2,p2,lty=1,length=0.1,angle=15,col="#666666")
      p <- p2
      start <- FALSE      
    }
  }
  
  options(warn = 0)
}

# exemplo
# VAmax <- 0.8
# VBmax <- 0.5
# pdualinhasfncV_eq(VAmax,VBmax)