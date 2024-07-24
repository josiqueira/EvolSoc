# vamos criar populações aleatórias com n indivíduos
# os indivíduos são de dois tipos, A e B, em quantidades aleatórias

n <- 137

# cria e exibe a populacao
populacao <- sample(c("A","B"), size=n, replace=TRUE)
cat("Populacao: ",populacao,"\n")

# mostra a proporcao de A e B
nA <- sum(populacao=="A")
nB <- sum(populacao=="B")
pA <- nA/n
pB <- nB/n
cat(nA," individuos, pA: ",pA,"\n",sep="")
cat(nB," individuos, pB: ",pB,"\n",sep="")
cat ("\nResulta no produto pA*pB = p(1-p) = ",pA*pB,"\n",sep="")

# substituindo A e B por números
cat ("\nSubstituindo A por 1 e B por 0 temos\n",sep="")
populacao[populacao=="A"] <- "1"
populacao[populacao=="B"] <- "0"
populacao <- as.numeric(populacao)
cat("Populacao: ",populacao,"\n")
cat("Como a população agora tem números, podemos computar a variância com:\n")
cat ("mean(populacao) = ",m <- mean(populacao),"\n",sep="")
cat ("soma dos (valores-media)^2 = ",s2 <- sum((populacao-m)^2),"\n",sep="")
cat ("var(populacao) = ",s2/n,"\n",sep="")
