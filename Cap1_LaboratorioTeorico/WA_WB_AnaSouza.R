## Solução Lista Ana Souza

V <- function (maxfit, p)
{
  fitness <- (maxfit*(exp(-p)-exp(-1)))/(1-exp(-1))
  return (fitness)}

deltapdec <- function (p, maxfitA, maxfitB, zA=1, zB=1)
  
{
  WA <- V(maxfitA,p)*zA
  WB <- V(maxfitB,(1-p))*zB
  dt <- data.frame(p,WA,WB)
  dt$w <- dt$p*dt$WA + (1-dt$p)*dt$WB
  dt$dp <- dt$p*(1-dt$p)*(dt$WA-dt$WB)/dt$w
  plot(dt$p, dt$dp, 
       xlab="p", ylab="delta p",
       xlim=c(0,1.05),
       ylim=c(min(dt$dp,na.rm=TRUE),max(dt$dp,na.rm=TRUE)*1.1),
       type="l")
  # maximo
  pmax <- p[which(dt$dp==max(dt$dp,na.rm=TRUE))]
  abline(v=pmax, lty=2)
  points(pmax,max(dt$dp,na.rm=TRUE),pch=21,col="black",bg="black")
  text(pmax,max(dt$dp,na.rm=TRUE)*1.05,paste0("p = ",round(pmax,4)),cex=0.7,pos=4)
  # minimo
  pmin <- p[which(dt$dp==min(dt$dp,na.rm=TRUE))]
  abline(v=pmin, lty=2)
  points(pmin,min(dt$dp,na.rm=TRUE),pch=21,col="black",bg="black")
  text(pmin,min(dt$dp,na.rm=TRUE)*1.05,paste0("p = ",round(pmax,4)),cex=0.7,pos=4)
  return (dt$dp)
}

maxfitA <- 0.8
zA <- 1
maxfitB <- 0.5
zB <- 1
p <- seq(0,1,by=0.0001)

WA <- V(maxfitA,p)*zA
WB <- V(maxfitB,(1-p))*zB
plot(p,WA,
     main="Fitness para os genótipos\n(exponenciais negativas)",
     xlab="p", ylab="W",
     xlim=c(0,1),ylim=c(0,max(WA+.05)),
     lwd=1, type="l") # plotando gráfico do comportamento de W(A) (Fitness da variante A) em função de p (proporção da variante A), quando Vmax= 0.8
lines (p,WB,
       lwd=1, lty=2, col="blue")  # plotando no gráfico o comportamento de W(B) (Fitness da variante B) em função de p (proporção da variante A), quando Vmax= 0.5. Obs: proporção das variantes A e B são complementares (A+B=1), por isso só utiliza-se os valores de p como referência 
legend ("topright", 
        c("A","B"),
        lwd=1,  
        lty=c(1,2), 
        col=c("black","blue"),
        bty="n",
        bg="transparent")  

dW <- c()

for (i in p) {
  WAp <- V(maxfitA,i)
  WBp <- V(maxfitB,(1-i))
  dW <- c(dW, WAp-WBp)
}

position <- which(abs(dW-0)==min(abs(dW-0)))
points(p[position],WA[position], pch=21, bg="blue")

cat(paste("Ponto de equilíbrio aproximadamente igual a", round(p[position],5)))

# fitA <- lm(WA~p)
# fitB <- lm(WB~p)
# 
# cm <- rbind(coef(fitA),coef(fitB))
# coord <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
# print(coord)
# points(coord[1], coord[2])
