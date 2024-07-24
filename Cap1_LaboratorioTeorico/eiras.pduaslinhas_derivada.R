# eiras.pduaslinhas_derivada.R

library(stats)

pduaslinhas_derivada <- function(VA,VB)
{
  # derivada
  dp <- stats::deriv(~p*VA/(p*VA+(1-p)*VB),"p")
  
  p <- seq(0,1,length.out = 100)
  dv <- eval(dp)
  slope <- attr(dv,"gradient")
  ymin <- min(-1,slope)
  ymax <- max( 1,slope)
  plot(p,slope,
       main="Derivada de p''=pV(A)/(pV(A)+(1-p)V(B))",
       xlab="p",ylab="derivada",
       xlim=c(-0.1,1.1),
       ylim=c(ymin,ymax),
       lwd=2,col="blue",type="l")
  abline(h=c(-1,1),lty=2)
  
  # pontos de equilibrio  
  ps <- c(0,1)
  for (p in ps)
  {
    dv <- eval(dp)
    slope <- attr(dv,"gradient")
    cor <- "white"
    if (slope>-1 & slope<1) {cor <- "black"}
    points(p,slope,pch=21,col="black",bg=cor)
    text(p,slope,round(slope,3),pos=1)
  }
}

