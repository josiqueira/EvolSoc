source("eiras.Vmax.R")

all_ps <- seq(0,1,length.out=100)
f <- V(maxfit=0.8, p=all_ps)
plot(all_ps,f,
     main="Viabilidade para os genótipos\n(exponenciais negativas)",
     xlab="p", ylab="V",
     xlim=c(0,1),ylim=c(0,1),
     lwd=2, type="l")
f <- V(maxfit=0.5, p=all_ps)
lines (all_ps,f,
       lwd=2, lty=2, col="blue")
legend ("topright",	
        c("A","B"),
        lwd=2,	
        lty=c(1,2),	
        col=c("black","blue"),
        bty="n",
        bg="transparent")	
