library(openxlsx)

source("eiras.viabilidadeplot.R")

source("eiras.friendlycolor.R")
source("eiras.Vmax.R")

##################################################################
# altere a configuracao aqui

# parametros
v <- 1
d <- 10 # distancia maxima para que o individuo perceba o alimento
# quantidade de recursos colocados no ambiente por ciclo
v.por.ciclo <- 50
# para nao acumular muito alimento, quantos ciclos em media o alimento dura
avg.ciclos.d <- 5

# tamanho do ambiente
env.x <- 240
env.y <- 240
# distância máxima da variação da posicao do individuo por ciclo
s <- 10

# populacao inicial
populacao <- 300 # tamanho 
alimento <- 10 # alimento de cada individuo
# esta eh a quantidade de alimento que um novo nascido deve receber
# portanto, quando um indivíduo tem excedente 2*alimento ou mais,
# entao tem probabilidade crescente de gerar outro individuo

# composicao da populacao
tipos <- c("A","B") # possiveis genótipos
proporcao <- c(0,1) # proporcao inicial dos genótipos  c(H,D)
cores <- c(30,10) # tijolo e azul
decaimento <- c(0.5,0.8) # [0,1] d para exp(-dp)
z <- c(1,1) # proporcao de zigotos viaveis

# probabilidade de mutacao para o novo individuo
probmuta <- 0.005
ciclos.muta <- 2000 # a partir de qual ciclo mutacoes podem ocorrer

# tempo de vida medio (numero inteiro maior que 1)
avgage <- 200
# mortalidade extrinseca = 1/avgage
probmorte.ind <- 1/avgage 

# metabolismo (consumo do individuo por ciclo)
metabolismo.ind <- 0.0

# duracao total da simulacao
ciclos <- 10000
# a cada quantos ciclos exibe o ambiente
ciclos.show <- 100
# repeticoes da simulacao
repeticoes <- 2

##################################################################

# nao altere daqui em diante
dt_historiatotal_populacao <- data.frame(matrix(nrow=ciclos*repeticoes, ncol=4))
names(dt_historiatotal_populacao) <- c("ciclo","A","B","total")
dt_historiatotal_populacao$ciclo <- 1:ciclos
popidx <- 0
maxpopulacao <- 1

for (rep.aux in 1:repeticoes)
{
  namespop <- c("x","y","tipo","alimento","prob","ocorre")
  proporcao <- proporcao/sum(proporcao) # garante somar 1
  popaux <- round(proporcao*populacao,0)
  populacao <- sum(popaux) # evita problemas com o arredondamento
  popini <- c()
  for (p.aux in 1:length(popaux))
  {
    popini <- c(popini,rep(tipos[p.aux],popaux[p.aux]))
  }
  dt_populacao <- data.frame(runif(populacao,1,env.x),
                             runif(populacao,1,env.y),
                             popini,
                             alimento,
                             0,0)
  names(dt_populacao) <- namespop
  # embaralha a ordem dos individuos
  dt_populacao$ocorre <- runif(nrow(dt_populacao))
  dt_populacao <- dt_populacao[order(dt_populacao$ocorre),]
  
  namesalm <- c("x","y","ordem","ind_1")
  dt_alimento <- data.frame(matrix(nrow=0,ncol=length(namesalm)))
  names(dt_alimento) <- namesalm
  passoalm <- 1/(2*alimento-1) 
  
  dt_historia_populacao <- data.frame(matrix(nrow=round(ciclos/100), ncol=4))
  names(dt_historia_populacao) <- c("ciclo","A","B","total")
  
  for (ciclo in 0:ciclos)
  {
    popidx <- popidx+1 # indice do historico
    # movimento
    dt_populacao$x <- dt_populacao$x+sample(seq(-s,s,length.out=100),size=nrow(dt_populacao),replace=TRUE)
    dt_populacao$x[dt_populacao$x<1] <- env.x-abs(dt_populacao$x[dt_populacao$x<1])
    dt_populacao$x[dt_populacao$x>env.x] <- dt_populacao$x[dt_populacao$x>env.x]-env.x
    dt_populacao$y <- dt_populacao$y+sample(seq(-s,s,length.out=100),size=nrow(dt_populacao),replace=TRUE)
    dt_populacao$y[dt_populacao$y<1] <- env.y-abs(dt_populacao$y[dt_populacao$y<1])
    dt_populacao$y[dt_populacao$y>env.y] <- dt_populacao$y[dt_populacao$y>env.y]-env.y
    
    # distribuicao do alimento
    dt_tmp <- data.frame(runif(v.por.ciclo,1,env.x),
                         runif(v.por.ciclo,1,env.y),
                         0,0,0)
    names(dt_tmp) <- namesalm
    dt_alimento <- rbind(dt_alimento,dt_tmp)
    
    # embaralha a ordem dos individuos e do alimento
    dt_populacao$ocorre <- runif(nrow(dt_populacao))
    dt_populacao <- dt_populacao[order(dt_populacao$ocorre),]
    dt_alimento$ordem <- runif(nrow(dt_alimento))
    dt_alimento <- dt_alimento[order(dt_alimento$ordem),]
    
    dt_populacao$ocorre <- 0 # todos livres (marca 1 se participa de diade)
    dt_alimento$ind_1 <- NA 
    # os individuos se alimentam
    for (d.aux in 1:nrow(dt_alimento))
    {
      # sorteia um individuo que alcanca este alimento
      inds <- which (abs(dt_alimento$x[d.aux] - dt_populacao$x ) <= d &
                       abs(dt_alimento$y[d.aux] - dt_populacao$y ) <= d &
                       dt_populacao$ocorre==0)
      if (length(inds)>=2)
      {
        inds <- sample(inds,size=1,replace=FALSE)
      }
      dt_populacao$ocorre[inds] <- 1 # nao pode mais entrar neste ciclo
      dt_alimento$ind_1[d.aux] <- inds[1]
    }
    dt_tmp <- dt_alimento[!is.na(dt_alimento$ind_1),]
    if(nrow(dt_tmp)>0)
    {
      dt_populacao$alimento[dt_tmp$ind_1] <- dt_populacao$alimento[dt_tmp$ind_1]+v
    }
    # metabolismo basal
    dt_populacao$alimento <- dt_populacao$alimento-metabolismo.ind
    
    if ((ciclo%%ciclos.show) == 0)
    {
      o.par <- par()
      layout(matrix(c(1,2,1,3),nrow=2,ncol=2,byrow=TRUE),widths=c(2,1))
      
      # exibe a populacao
      plot(NA, xlim=c(-20,env.x), ylim=c(0,env.y),
           main=paste0("ciclo ",ciclo,", v=",v),
           xlab="",ylab="",axes=FALSE)
      for (t.aux in 1:length(tipos))
      {
        points(dt_populacao$x[dt_populacao$tipo==tipos[t.aux]],
               dt_populacao$y[dt_populacao$tipo==tipos[t.aux]], 
               pch=21,col="black",bg=paste0(friendlycolor(cores[t.aux]),"88"),cex=1,lwd=0.5)
      }
      # alimento disponivel
      points(dt_alimento$x,
             dt_alimento$y, 
             pch=21,col="black",bg="black",
             cex=0.5)
      # liga alimento com consumo unico
      dt_tmp <- dt_alimento[!is.na(dt_alimento$ind_1),]
      if (nrow(dt_tmp)>0)
      {
        cor <- friendlycolor(21)
        for (d.aux in 1:nrow(dt_tmp))
        {
          lines (
            c( dt_populacao$x[dt_tmp$ind_1[d.aux]],
               dt_tmp$x[d.aux]),
            c( dt_populacao$y[dt_tmp$ind_1[d.aux]],
               dt_tmp$y[d.aux]),
            col=cor,lwd=1
          )
        }
      }
    } # show
    
    # mantem somente os alimentos nao consumidos (nao usados por individuos)
    dt_alimento <- dt_alimento[is.na(dt_alimento$ind_1),]
    # elimina alimentos por probabilidade (inverso da duracao media)
    dt_alimento$ordem <- runif(nrow(dt_alimento))
    dt_alimento <- dt_alimento[dt_alimento$ordem>=1/avg.ciclos.d,]
    
    # nascimentos
    # para ter no máximo um filhote por vez, 
    # a probabilidade de ter filhote 
    # eh minima quando o indivíduo tem 2*alimento
    # e probabilidade igual a 1 quando tem 3*alimento
    dt_populacao$prob <- (dt_populacao$alimento*0.1-1)*passoalm
    dt_populacao$prob[dt_populacao$prob>1] <- 1
    dt_populacao$prob[dt_populacao$prob<0] <- 0
    dt_populacao$ocorre <- runif(nrow(dt_populacao))
    dt_populacao$ocorre[dt_populacao$prob>dt_populacao$ocorre] <- 1
    dt_populacao$ocorre[dt_populacao$ocorre!=1] <- 0
    # perda de gametas por 1-z
    # gera os filhotes
    dt_tmp <- dt_populacao[dt_populacao$ocorre==1,]
    if (nrow(dt_tmp)>0)
    {
      dt_tmp$prob <- runif(nrow(dt_tmp))
      for (t.aux in 1:length(tipos))
      {
        dt_tmp$ocorre[dt_tmp$tipo==tipos[t.aux] & dt_tmp$prob<=(1-z[t.aux])] <- 0 
      }
      dt_tmp <- dt_tmp[dt_tmp$ocorre==1,]
    }
    # retira alimento dos parentais
    dt_populacao$alimento[dt_populacao$ocorre==1] <- dt_populacao$alimento[dt_populacao$ocorre==1] - 
      dt_populacao$alimento[dt_populacao$ocorre==1]*0.1
    if(nrow(dt_tmp)>0)
    {
      dt_tmp$alimento <- dt_tmp$alimento*0.1  # coloca o alimento nos filhotes
      if (ciclo>ciclos.muta)
      {
        # mutacoes dos filhotes0.005
        dt_tmp$prob <- runif(nrow(dt_tmp))
        dt_tmp$ocorre <- "0"
        dt_tmp$ocorre[dt_tmp$prob<=probmuta] <- "1" # marca os mutantes
        dt_tmp$ocorre[dt_tmp$ocorre=="1" & dt_tmp$tipo=="A"] <- "B"
        dt_tmp$ocorre[dt_tmp$ocorre=="1" & dt_tmp$tipo=="B"] <- "A"
        dt_tmp$tipo[dt_tmp$ocorre!="0"] <- dt_tmp$ocorre[dt_tmp$ocorre!="0"]
      }
      # incorpora os nascidos na populacao
      dt_populacao <- rbind(dt_populacao,dt_tmp)
      rownames(dt_populacao)<-NULL # mantem os nomes da linhas limpos
    } # nascimentos
    
    if ((ciclo%%ciclos.show) == 0)
    {
      # distribuicao do alimento por genotipo
      maxx <- 0
      maxy <- 0
      for (g.aux in 1:length(tipos))
      {
        valores <- dt_populacao$alimento[dt_populacao$tipo==tipos[g.aux]]
        if (length(valores)>2)
        {
          densidade <- density(valores,na.rm=TRUE)
          densidade$y <- densidade$y*length(valores)
          if (maxx < max(densidade$x)) {maxx <- max(densidade$x)}
          if (maxy < max(densidade$y)) {maxy <- max(densidade$y)}
        }
      }
      plot(NA,
           xlab="alimento", ylab="densidade x individuos",
           xlim=c(0,maxx),ylim=c(0,maxy))
      t.leg <- c.leg <- c()
      for (g.aux in 1:length(tipos))
      {
        valores <- dt_populacao$alimento[dt_populacao$tipo==tipos[g.aux]]
        if (length(valores)>2)
        {
          densidade <- density(valores,na.rm=TRUE)
          densidade$y <- densidade$y*length(valores)
          cor <- friendlycolor(cores[g.aux])
          lines(densidade, col=cor)
          t.leg <- c(t.leg,paste0(tipos[g.aux],": ",length(valores)," (",round(length(valores)/nrow(dt_populacao)*100,1),")%"))
          c.leg <- c(c.leg,cor)
        }
      }
      legend ("topleft",	
              t.leg,
              lwd=1,	
              lty=1,	
              col=c.leg,	
              box.lwd=0,	
              bg="transparent")	
    } # show
    
    # probabilidade basal de morrer
    dt_populacao$ocorre <- 0
    dt_populacao$prob <- runif(nrow(dt_populacao))
    dt_populacao$ocorre[dt_populacao$prob<=probmorte.ind] <- 1
    # probabilidade de morrer pelo genotipo (V ... viabilidade)
    dt_populacao$prob <- runif(nrow(dt_populacao))
    for (t.aux in 1:length(tipos))
    {
      # calcula a viabilidade com o valor corrente
      proporcao <- sum(dt_populacao$tipo==tipos[t.aux],na.rm=TRUE)/sum(!is.na(dt_populacao$tipo))
      viabilidade <- exp(-decaimento[t.aux]*proporcao)
      # a viabilidade dos individuos, portanto, depende da probabilidade
      # do individuo sobreviver para reproduzir: a probabilidade de morrer
      # a cada ciclo é, apenas, a subdivisao de (1-V) em avgage passos
      probmorte.V <- (1-viabilidade)/avgage
      dt_populacao$ocorre[dt_populacao$tipo==tipos[t.aux] & dt_populacao$prob<=probmorte.V] <- 1
    }
    # mantem os que nao morreram (extrinseco ou por genotipo)
    dt_populacao <- dt_populacao[dt_populacao$ocorre==0,]
    # mortes dos que nao tem mais alimento suficiente
    dt_populacao <- dt_populacao[dt_populacao$alimento>0,]
    
    # guarda o historico
    if(sum(is.na(dt_historia_populacao$ciclo))>0) # ainda tem espaco vazio
    {
      # pega o primeiro vazio que encontra
      linhistoria <- which(is.na(dt_historia_populacao$ciclo))[1]
    } else
    {
      # substitui o mais antigo
      linhistoria <- which(dt_historia_populacao$ciclo==min(dt_historia_populacao$ciclo,na.rm=TRUE))
    }
    dt_historia_populacao$ciclo[linhistoria] <- ciclo
    dt_historia_populacao[linhistoria,2:ncol(dt_historia_populacao)] <- 0
    pops <- table(dt_populacao$tipo)
    total <- sum(pops)
    dt_historia_populacao$total[linhistoria] <- total
    dt_historiatotal_populacao$total[popidx] <- total
    if (maxpopulacao < total) {maxpopulacao <- total}
    for (p.aux in 1:length(pops))
    {
      dt_historia_populacao[linhistoria, which(names(dt_historia_populacao)==names(pops)[p.aux])] <- as.numeric(unlist(pops[p.aux]))
      # guarda tambem no historico completo
      dt_historiatotal_populacao[popidx, which(names(dt_historiatotal_populacao)==names(pops)[p.aux])] <- as.numeric(unlist(pops[p.aux]))
    }
    dt_historia_populacao <- dt_historia_populacao[order(dt_historia_populacao$ciclo),]
    
    
    if ((ciclo%%ciclos.show) == 0)
    {
      # proporcao de cada genotipo
      if(sum(!is.na(dt_historia_populacao$ciclo))>0)
      {
        plot(NA,
             col=friendlycolor(as.numeric(names(dt_historia_populacao)[2])),
             xlab="ciclos", ylab="população",
             xlim=c(dt_historia_populacao$ciclo[1],ciclo),
             ylim=c(0,max(dt_historia_populacao$total,na.rm=TRUE)),
             # ylim=c(0,maxpopulacao),
             type="l"
        )
        t.leg <- c.leg <- c()
        for (c.aux in ncol(dt_historia_populacao):2)
        {
          cor <- "black"
          lwd <- 2
          if (c.aux < ncol(dt_historia_populacao))
          {
            c2.aux <- which(tipos==names(dt_historia_populacao)[c.aux])
            cor <- friendlycolor(cores[c2.aux])
            lwd <- 1
          }
          lines(dt_historia_populacao$ciclo,
                dt_historia_populacao[,c.aux],
                lwd=lwd, col=cor)
          t.leg <- c(t.leg,names(dt_historia_populacao)[c.aux])
          c.leg <- c(c.leg,cor)
        }
        legend ("topleft",	
                t.leg,
                lwd=1,	
                lty=1,	
                col=c.leg,	
                box.lwd=0,	
                bg="transparent")	
      }
    } # show 
    
    
    if ((ciclo%%ciclos.show) == 0)
    {
      # restaura o layout
      par(o.par)
      if (ciclos.show < 10)
      {
        Sys.sleep(0.3)
      }
    } # show 
  }
  
} # for rep.aux

openxlsx::write.xlsx(dt_historiatotal_populacao,"viabilidadedec_res.xlsx")
cat("\nTrajetorias guardadas em viabilidadedec_res.xlsx\n")

viabilidadeplot("viabilidadedec_res.xlsx")
