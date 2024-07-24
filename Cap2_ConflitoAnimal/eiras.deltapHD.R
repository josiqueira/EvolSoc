# eiras.deltapHD.R

source("eiras.HD.R")

deltapHD <- function (p, w0, v, c, graph=TRUE)
{
  WH <- W("H", p, w0, v, c)
  WD <- W("D", p, w0, v, c)
  dt <- data.frame(p,WH,WD)
  dt$w <- dt$p*dt$WH + (1-dt$p)*dt$WD
  dt$dp <- dt$p*(1-dt$p)*(dt$WH-dt$WD)/dt$w
  if(graph)
  {
    plot(dt$p, dt$dp, 
         xlab="p", ylab="delta p",
         xlim=c(0,1.05),
         ylim=c(min(dt$dp,na.rm=TRUE),max(dt$dp,na.rm=TRUE)*1.1),
         type="l")
    # # maximo
    # pmax <- p[which(dt$dp==max(dt$dp,na.rm=TRUE))]
    # abline(v=pmax, lty=2)
    # points(pmax,max(dt$dp,na.rm=TRUE),pch=21,col="black",bg="black")
    # text(pmax,max(dt$dp,na.rm=TRUE)*1.05,paste0("p = ",round(pmax,4)),cex=0.7,pos=4)
    # # minimo
    # pmin <- p[which(dt$dp==min(dt$dp,na.rm=TRUE))]
    # abline(v=pmin, lty=2)
    # points(pmin,min(dt$dp,na.rm=TRUE),pch=21,col="black",bg="black")
    # text(pmin,min(dt$dp,na.rm=TRUE)*1.05,paste0("p = ",round(pmax,4)),cex=0.7,pos=4)
  }
  return (dt$dp)
}
