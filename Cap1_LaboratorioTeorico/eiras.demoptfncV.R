# eiras.demoptfncV.R

source("eiras.Vmax.R")

p0 <- 0.1
VAmax <- 0.8
VBmax <- 0.5
t <- seq(1,20,1)
plot(0,p0,
     type="p", 
     main=paste0("Evolucao da proporcao de A\n",
                 "p0 = ",p0,", V(A) = ",VAmax,", V(B) = ",VBmax), 
     xlim=c(0,max(t)),ylim=c(0,1),
     ylab="p(t)", xlab="t",col="black",bg="white")
p <- p0
for (t.aux in t)
{
  oldp <- p
  deltap <- p*(1-p)*((V(VAmax,p)-V(VBmax,(1-p))) / ( p*V(VAmax,p) + (1-p)*V(VBmax,(1-p)) ) )
  p <- p+deltap
  lines(c(t.aux-1,t.aux), c(oldp,p))
  points(t.aux,p,pch=21,col="black",bg="white")
}
