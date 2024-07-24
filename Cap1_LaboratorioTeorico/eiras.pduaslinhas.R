# eiras.pduaslinhas.R

# funcao p''
pdualinhas <- function(VA,VB)
{
  
  curve(x*VA/(x*VA+(1-x)*VB),0,1,
        main=paste0("Funcao recursiva de p''\nV(A)=",VA,", V(B)=",VB),
        xlab="p",ylab="p''")
  # bissetriz
  curve(x^1,0,1,lty=2,add=TRUE)
  # legenda
  legend ("topleft",	
          c("p''=pV(A)/(pV(A)+(1-p)V(B))",
            "p'' = p"),
          lwd=1,	
          lty=c(1,2),	
          cex=0.7,
          bty="n",
          bg="transparent")	
  
  # acha os pontos de equilibro, onde p=p''
  for (p in seq(0,1,length.out = 1e4))
  {
    p2 <- p*VA/(p*VA+(1-p)*VB)
    if (round(p2,4)==round(p,4))# exemplo
      # VA <- 0.8
      # VB <- 0.5
      # pdualinhas_eq(VA,VB)
    {
      points(p,p2,pch=21,col="#aaaaaa",bg="#aaaaaa",cex=0.8)
    }
  }
}

# exemplo
# VA <- 0.8
# VB <- 0.5
# pdualinhas(VA,VB)