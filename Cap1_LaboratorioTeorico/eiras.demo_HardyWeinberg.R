# Hardy-Weinberg
source("eiras.friendlycolor.R")
source("eiras.jitter.R")

# fixed seed or random start
# seed <- 0.7977821
# cat("Initial seed:",seed,"\n")
# set.seed(seed)

hw_teor <- function()
{
  # teórico
  fA <- fB <- fAA <- fAB <- fBB <- c()
  f <- seq(0,1,0.01)
  for (p in f)
  {
    fA <- c(fA,p)
    fB <- c(fB,1-p)
    fAA <- c(fAA,p^2)
    fAB <- c(fAB,2*p*(1-p))
    fBB <- c(fBB,(1-p)^2)
  }
  lines(f,fAA,col="#666666",lty=2,lwd=2)
  lines(f,fAB,col="#666666",lty=1,lwd=2)
  lines(f,fBB,col="#666666",lty=3,lwd=2)
  legend("right",
         c("AA", "AB", "BB"),
         lwd=2,
         lty=c(2,1,3),
         box.lwd=0, bg="transparent")
}

# simulado (VA > VB, iniciando com populacao de BB)
pop_n <- 50
fA <- 0
fB <- 1-fA
probdeathA <- 0.025
probdeathB <- 0.05
mutacao <- 0.01
last_cycle <- 3000

# grafico vazio
plot(NA,xlim=c(0,1),ylim=c(0,1),
     main=paste("Simulacao de Hardy-Weinberg\np²+2pq+q² ... n = ",pop_n,sep=""),
     xlab="proporção de alelos A",
     ylab="proporção de genótipos")

# linhas de H-W teoricas
hw_teor()

# populacao inicial
pop <- c()
for (i in 1:pop_n)
{
  genotipo <- ""
  if (runif(1)<=fA)
  {genotipo <- paste(genotipo,"A",sep="")} else
  {genotipo <- paste(genotipo,"B",sep="")} 
  if (runif(1)<=fA)
  {genotipo <- paste(genotipo,"A",sep="")} else
  {genotipo <- paste(genotipo,"B",sep="")} 
  pop <- c(pop,genotipo)
}
df_pop <- data.frame(1:length(pop),pop,1,0)
names(df_pop) <- c("i","genotipo","status","probdeath")
df_pop$probdeath[df_pop$genotipo=="AA"] <- probdeathA
df_pop$probdeath[df_pop$genotipo=="AB" | df_pop$genotipo=="BA"] <- (probdeathA+probdeathB)/2
df_pop$probdeath[df_pop$genotipo=="BB"] <- probdeathB
n <- length(pop)
fAA <- sum(df_pop$genotipo=="AA")/n
fAB <- sum(df_pop$genotipo=="AB" | df_pop$genotipo=="BA")/n
fBB <- sum(df_pop$genotipo=="BB")/n
# legenda
col.fAA <- paste(friendlycolor(20),sep="")
col.fAB <- paste(friendlycolor(14),sep="")
col.fBB <- paste(friendlycolor(8),sep="")
legend("left",
       c("AA", "AB", "BB"),
       lty=0,
       pch=c(24,23,25),
       col=c(col.fAA,col.fAB,col.fBB),
       pt.bg="white",
       box.lwd=0, bg="transparent")

# simulation
cycle <- 1
cat("cycle","\t","fA","\t","fB","\t","fAA","\t","fAB","\t","fBB","\n")
cat(cycle,
    "\t",sprintf("%.3f",fA),
    "\t",sprintf("%.3f",1-fA),
    "\t",sprintf("%.3f",fAA),
    "\t",sprintf("%.3f",fAB),
    "\t",sprintf("%.3f",fBB),"\n")
colstore <- c(cycle,"fA","fB","fAA","fAB","fBB")
df_storedata <- data.frame(matrix(nrow = 0, ncol = length(colstore)))
names(df_storedata) <- colstore
while (cycle < last_cycle)
{
  n <- length(pop)
  fAA <- sum(df_pop$genotipo=="AA")/n
  fAB <- sum(df_pop$genotipo=="AB" | df_pop$genotipo=="BA")/n
  fBB <- sum(df_pop$genotipo=="BB")/n
  fA <- (2*fAA+fAB)/2
  df_tmp <- data.frame(cycle,fA,1-fA,fAA,fAB,fBB)
  names(df_tmp) <- colstore
  df_storedata <- rbind(df_storedata,df_tmp)
  # morte
  df_pop$status <- runif(nrow(df_pop))
  df_pop <- df_pop[df_pop$status > df_pop$probdeath,]
  df_pop$i <- 1:nrow(df_pop)
  # nascimento
  while(nrow(df_pop) < pop_n)
  {
    # reproducao, sorteia dois indivíduos diferentes
    i1 <- round(runif(1,1,nrow(df_pop)),0)
    i2 <- i1
    while(i2==i1)
    {
      i2 <- round(runif(1,1,nrow(df_pop)),0)
    }
    df_tmp <- df_pop[df_pop$i==i1 | df_pop$i==i2,]
    gametas <- c()
    g1 <- as.character(df_tmp$genotipo[1])
    a <- round(runif(1,1,2),0); gametas <- c(gametas,substr(g1,a,a))
    a <- round(runif(1,1,2),0); gametas <- c(gametas,substr(g1,a,a))
    g2 <- as.character(df_tmp$genotipo[2])
    a <- round(runif(1,1,2),0); gametas <- c(gametas,substr(g2,a,a))
    a <- round(runif(1,1,2),0); gametas <- c(gametas,substr(g2,a,a))
    # mutacao
    for (a in 1:length(gametas))
    {
      if (runif(1)<=mutacao)
      {
        if (gametas[a]=="A") 
        {gametas[a]<-"B"} else
        {gametas[a]<-"A"}
      }
    }
    # recebe 1 ou 2, 3 ou 4
    newgenotipo <- ""
    a <- round(runif(1,1,2),0)
    newgenotipo <- paste(newgenotipo,gametas[a],sep="")
    a <- round(runif(1,3,4),0) 
    newgenotipo <- paste(newgenotipo,gametas[a],sep="")
    df_tmp <- data.frame(max(df_pop$i)+1, newgenotipo, 1, 0)
    names(df_tmp) <- c("i","genotipo","status","probdeath")
    df_tmp$probdeath[df_tmp$genotipo=="AA"] <- probdeathA
    df_tmp$probdeath[df_tmp$genotipo=="AB" | df_tmp$genotipo=="BA"] <- (probdeathA+probdeathB)/2
    df_tmp$probdeath[df_tmp$genotipo=="BB"] <- probdeathB
    df_pop <- rbind(df_pop,df_tmp)
  } # while nascimento
  df_pop$i <- 1:nrow(df_pop)
  cycle <- cycle+1
  if ( (cycle %% 100) == 0)
  {
    cat(cycle,
        "\t",sprintf("%.3f",fA),
        "\t",sprintf("%.3f",1-fA),
        "\t",sprintf("%.3f",fAA),
        "\t",sprintf("%.3f",fAB),
        "\t",sprintf("%.3f",fBB),"\n")
    lines(df_storedata$fA,df_storedata$fAA,pch=24,cex=0.8,col=col.fAA,bg="white",type="b")
    lines(df_storedata$fA,df_storedata$fAB,pch=23,cex=0.8,col=col.fAB,bg="white",type="b")
    lines(df_storedata$fA,df_storedata$fBB,pch=25,cex=0.8,col=col.fBB,bg="white",type="b")
  }
} # while not end

# final graph
# grafico teorico
plot(NA,xlim=c(0,1),ylim=c(0,1),
     main=paste("Simulacao de Hardy-Weinberg\np²+2pq+q² ... n = ",pop_n,sep=""),
     xlab="proporção de alelos A",
     ylab="proporção de genótipos")
lines(df_storedata$fA,df_storedata$fAA,pch=24,cex=0.8,col=col.fAA,bg="white",type="b")
lines(df_storedata$fA,df_storedata$fAB,pch=23,cex=0.8,col=col.fAB,bg="white",type="b")
lines(df_storedata$fA,df_storedata$fBB,pch=25,cex=0.8,col=col.fBB,bg="white",type="b")
cat(cycle,
    "\t",sprintf("%.3f",fA),
    "\t",sprintf("%.3f",1-fA),
    "\t",sprintf("%.3f",fAA),
    "\t",sprintf("%.3f",fAB),
    "\t",sprintf("%.3f",fBB),"\n")
legend("left",
       c("AA", "AB", "BB"),
       lty=0,
       pch=c(24,23,25),
       col=c(col.fAA,col.fAB,col.fBB),
       pt.bg="white",
       box.lwd=0, bg="transparent")
# reexibe as linhas teóricas
hw_teor()

# p vs delta p
delta_p_teorico <- c()
delta_p <- c()
p <- c()
vA = 1-probdeathA
vB = 1-probdeathB
for (r in 2:nrow(df_storedata))
{
  p <- c(p,df_storedata$fA[r])
  delta_p <- c(delta_p, df_storedata$fA[r]-df_storedata$fA[r-1])
  fA <- df_storedata$fA[r]
  dpt <- fA*(1-fA)*((vA-vB)/(fA*vA+(1-fA)*vB))
  delta_p_teorico <- c(delta_p_teorico, dpt)
}
delta_p_j <- ps_jitter(delta_p)
plot (p,delta_p_j,
      main=paste("n = ",pop_n,sep=""),
      type="b", cex=0.5, col=friendlycolor(41))
points(p,delta_p_j,cex=0.5, pch=21, col="black",bg="black")
lines(p,delta_p_teorico,lty=2, lwd=2)
abline(h=0,lty=4)

# t vs delta p
t <- c()
p <- c()
for (r in 2:nrow(df_storedata))
{
  t <- c(t,r-1)
  p <- c(p,df_storedata$fA[r])
}
plot (t,p, ylim=c(0,1),
      main=paste("n = ",pop_n,sep=""),
      type="l", lwd=2)

