# eiras.pduaslinhas_dv.R

library(stats)

pduaslinhas_dv <- function(VA,VB,ps)
{
  titulo <- paste0("Exemplos de ",length(ps)," derivadas\np = {")
  virgula <- ""
  for (p in ps)
  {
    titulo <- paste0(titulo,virgula,p)
    virgula <- ", "
  }
  titulo <- paste0(titulo,"}")
  p <- seq(0,1,length.out = 100)
  p2 <- p*VA/(p*VA+(1-p)*VB)
  plot(p,p2,
       main=titulo,
       xlim=c(0,1),ylim=c(0,1),
       xlab="p", ylab="p''",
       type="l")
  lines(c(0,1),c(0,1),lty=2)
  dp <- stats::deriv(~p*VA/(p*VA+(1-p)*VB),"p")
  for (p in ps)
  {
    py <- p*VA/(p*VA+(1-p)*VB)
    dv <- eval(dp)
    slope <- as.vector(attr(dv,"gradient"))
    x1 <- p+c(-0.17,0,0.17)
    y1 <- x1*slope
    dif <- py-y1[2]
    points(x1[2],y1[2]+dif,pch=20,col="red")
    lines(x1,y1+dif,lwd=2,col="red")
  }
}

# VA<-0.8
# VB<-0.5
# # exemplos de algumas derivadas de p
# ps <- seq(0,1,by=0.4)
# pduaslinhas_dv(VA,VB,ps)

# # derivada
# p <- seq(0,1,length.out = 100)
# dv <- eval(dp)
# slope <- attr(dv,"gradient")
# ymin <- min(-1,slope)
# ymax <- max( 1,slope)
# plot(p,slope,
#      main="Derivada de p''=pV(A)/(pV(A)+(1-p)V(B))",
#      xlab="p",ylab="derivada",
#      ylim=c(ymin,ymax),
#      lwd=2,col="blue",type="l")
# abline(h=c(-1,1),lty=2)
# 
