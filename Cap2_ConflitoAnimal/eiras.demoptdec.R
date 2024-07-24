# eiras.demoptdec.R

source("eiras.dec.R")

p0 <- 0.1
VAdec <- 0.5
VBdec <- 0.8
t <- seq(1,60,1)
plot(0,p0,
     type="p", 
     main=paste0("Evolucao da proporcao de A\n",
                 "p0 = ",p0,", d_A = ",VAdec,", d_B = ",VBdec), 
     xlim=c(0,max(t)),ylim=c(0,1),
     ylab="p(t)", xlab="t",col="black",bg="white")
p <- p0
for (t.aux in t)
{
  oldp <- p
  deltap <- p*(1-p)*((V(VAdec,p)-V(VBdec,(1-p))) / ( p*V(VAdec,p) + (1-p)*V(VBdec,(1-p)) ) )
  p <- p+deltap
  lines(c(t.aux-1,t.aux), c(oldp,p))
  points(t.aux,p,pch=21,col="black",bg="white")
}
