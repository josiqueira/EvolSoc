p0 <- 0.01
VA <- 0.8
VB <- 0.75
t <- seq(0,144,1)
p <- 1/(1 + ((1 - p0)/p0)*((VB/VA)^t))
plot(t, p, 
     type="p", 
     main="Curva de crescimento logistica discreta", 
     sub=paste0("p0=",p0,", V(A)=",VA,", V(B)=",VB),
     ylab="p(t)", 
     xlab="t",
     pch=21, col="black", bg="black", cex=0.5, axes = FALSE)
axis(1, at=seq(0,145, by=35))
axis(2, at=seq(0,1,by=0.25))
t_inflex <- -log(p0/(1-p0))/log(VA/VB)
pt_inflex <- 1/2
segments(t_inflex,0,t_inflex,.5, lty=2)
segments(0,.5,t_inflex,.5, lty=2)
cat("t de inflexao = ", round(t_inflex,3), " geracoes", sep="")
