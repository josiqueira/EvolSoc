p0 <- 0.01
VA <- 0.8
VB <- 0.5
t <- seq(0,20,1)
p <- 1/(1 + ((1 - p0)/p0)*((VB/VA)^t))
plot(t, p, 
     type="p", 
     main="Curva de crescimento logistica discreta", 
     sub=paste0("p0=",p0,", V(A)=",VA,", V(B)=",VB),
     ylab="p(t)", 
     xlab="t",
     pch=21, col="black", bg="black")
t_inflex <- -log(p0/(1-p0))/log(VA/VB)
pt_inflex <- 1/2
abline(v=t_inflex,lty=2)
abline(h=pt_inflex,lty=2)
cat("t de inflexao = ", t_inflex, " geracoes", sep="")
