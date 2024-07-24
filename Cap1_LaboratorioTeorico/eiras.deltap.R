# eiras.deltap.R
deltap <- function (p, VA, VB, zA=1, zB=1)
{
  WA <- VA*zA
  WB <- VB*zB
  w <- p*WA+(1-p)*WB
  dp <- p*(1-p)*(WA-WB)/w
  plot(p, dp, 
       xlab="p", ylab="delta p",
       xlim=c(0,1.05),
       ylim=c(0,max(dp)*1.1),
       type="l")
  pmax <- p[which(dp==max(dp))]
  abline(v=pmax, lty=2)
  points(pmax,max(dp),pch=21,col="black",bg="black")
  text(pmax,max(dp)*1.05,paste0("p = ",pmax),cex=0.7,pos=4)
  return (dp)
}
