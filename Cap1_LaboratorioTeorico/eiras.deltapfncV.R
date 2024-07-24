# eiras.deltapfncV.R

source("eiras.Vmax.R")

deltapfncV <- function (p, VAmax, VBmax, zA=1, zB=1)
{
  WA <- V(VAmax,p)*zA
  WB <- V(VBmax,1-p)*zB
  dt <- data.frame(p,WA,WB)
  dt$w <- dt$p*dt$WA + (1-dt$p)*dt$WB
  dt$dp <- dt$p*(1-dt$p)*(dt$WA-dt$WB)/dt$w
  plot(dt$p, dt$dp, 
       xlab="p", ylab="delta p",
       xlim=c(0,1.05),
       ylim=c(min(dt$dp,na.rm=TRUE),max(dt$dp,na.rm=TRUE)*1.1),
       type="l")
  pmax <- p[which(dt$dp==max(dt$dp,na.rm=TRUE))]
  abline(v=pmax, lty=2)
  points(pmax,max(dt$dp,na.rm=TRUE),pch=21,col="black",bg="black")
  text(pmax,max(dt$dp,na.rm=TRUE)*1.05,paste0("p = ",pmax),cex=0.7,pos=4)
  return (dt$dp)
}
