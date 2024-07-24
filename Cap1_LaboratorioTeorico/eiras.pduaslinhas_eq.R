# eiras.pduaslinhas_eq.R

pdualinhas_eq <- function(VA,VB,p0)
{
  options(warn = -1)
  
  # caminho recursivo, iniciando com p baixo e p alto
  pini <- c(0.1,0.9)
  
  for (v in pini)
  {
    # funcao p''
    curve(x*VA/(x*VA+(1-x)*VB),0,1,
          main=paste0("Funcao recursiva de p''\nV(A)=",VA,", 
                      V(B)=",VB),
          xlab="p",ylab="p''",
          lwd=2)
    # bissetriz
    curve(x^1,0,1,lty=2,add=TRUE)
    # legenda
    legend ("topleft",	
            c("p''=pV(A)/(pV(A)+(1-p)V(B))",
              "p'' = p"),
            lwd=c(2,1),	
            lty=c(1,2),	
            cex=0.7,
            bty="n",
            bg="transparent")	
    
    # pontos de equilibro onde p=p''
    for (p in seq(0,1,length.out = 1e4))
    {
      p2 <- p*VA/(p*VA+(1-p)*VB)
      if (round(p2,4)==round(p,4))
      {
        points(p,p2,pch=21,col="black",bg="white",cex=0.8)
      }
    }
    
    # caminho recursivo
      # ponto de partida
    p <- v
    p2 <- p*VA/(p*VA+(1-p)*VB)
    points(p,0,pch="x",col="#666666")
    arrows(p,0,p,p2,lty=1,col="#666666",length=0.1,angle=15)
    # lines(c(p,p),c(0,p2),lty=1,col="#666666")
    start <- TRUE
    while(1)
    {
      p2 <- p*VA/(p*VA+(1-p)*VB)
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
# VA <- 0.8
# VB <- 0.5
# pdualinhas_eq(VA,VB)