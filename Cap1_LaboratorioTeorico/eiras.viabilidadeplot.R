source("eiras.friendlycolor.R")

# filename="viabilidadedec2_res.xlsx"
# png=TRUE
viabilidadeplot <- function(filename,png=FALSE)
{
  # proporcao
  dt_historiatotal_populacao <- readxl::read_excel(paste0(filename),col_types = "numeric")
  cores <- c(30,10) # tijolo e azul
  tipos <- c()
  for (c in 2:3)
  {
    o.names <- names(dt_historiatotal_populacao)
    dt_historiatotal_populacao$new <- dt_historiatotal_populacao[,c]/dt_historiatotal_populacao$total
    names(dt_historiatotal_populacao) <- c(o.names,paste0("p",names(dt_historiatotal_populacao)[c]))
    tipos <- c(tipos,names(dt_historiatotal_populacao)[c])
  }
  tipos <- c(tipos,"total")
  
  ciclos_unique <- unique(dt_historiatotal_populacao$ciclo)
  dt_resumo <- data.frame(ciclos_unique)
  names(dt_resumo) <- "ciclos"
  medidas <- c("m","ll","ul")
  for (t.aux in 1:length(tipos))
  {
    for (m.aux in medidas)
    {
      oldnames <- names(dt_resumo)
      dt_resumo$new <- NA
      names(dt_resumo) <- c(oldnames,paste0(m.aux,"_",tipos[t.aux]))
    }
  }
  for (t.aux in 1:length(tipos))
  {
    cat("\n",tipos[t.aux],"\n")
    c.org <- which(names(dt_historiatotal_populacao)==tipos[t.aux])
    c.dst <- which(names(dt_resumo)==paste0(medidas[1],"_",tipos[t.aux]))
    for (r.aux in 1:nrow(dt_resumo))
    {
      if ((r.aux%%1000)==0) 
      {
        cat(".")
      }
      valores <- as.numeric(unlist(dt_historiatotal_populacao[dt_historiatotal_populacao$ciclo==dt_resumo$ciclos[r.aux],c.org]))
      m <- mean(valores,na.rm=TRUE)
      if(is.finite(m))
      {
        dt_resumo[r.aux,c.dst] <- m
        q <- quantile(valores,probs=c(0.025,0.975),na.rm=TRUE)
        dt_resumo[r.aux,c.dst+1] <- q[1]
        dt_resumo[r.aux,c.dst+2] <- q[2]
      } else
      {
        dt_resumo[r.aux,c.dst] <- NA
        dt_resumo[r.aux,c.dst+1] <- NA
        dt_resumo[r.aux,c.dst+2] <- NA
      }
    }
  }  
  # em proporcao
  for (t.aux in 1:(length(tipos)-1))
  {
    for (m.aux in medidas)
    {
      oldnames <- names(dt_resumo)
      dt_resumo$new <- NA
      names(dt_resumo) <- c(oldnames,paste0(m.aux,"_",tipos[t.aux],"_p"))
    }
  }
  
  c1 <- c()
  c2 <- c()
  for (t.aux in 1:length(tipos))
  {
    c1 <- c(c1,which(names(dt_resumo)==paste0("m_",tipos[t.aux])))
    c2 <- c(c2,which(names(dt_resumo)==paste0("m_",tipos[t.aux],"_p")))
  }
  for (t.aux in 1:(length(c1)-1))
  {
    dt_resumo[,c2[t.aux]] <- as.numeric(unlist(dt_resumo[,c1[t.aux]]))/as.numeric(unlist(dt_resumo[,c1[length(c1)]]))
    dt_resumo[,c2[t.aux]+1] <- as.numeric(unlist(dt_resumo[,c1[t.aux]+1]))/as.numeric(unlist(dt_resumo[,c1[length(c1)]]))
    dt_resumo[,c2[t.aux]+2] <- as.numeric(unlist(dt_resumo[,c1[t.aux]+2]))/as.numeric(unlist(dt_resumo[,c1[length(c1)]]))
  }
  
  # absoluto
  if (png)
  {
    png(paste0(filename,"_abs.png"), width=800, height=550)
  }
  maxx <- max(dt_resumo$ciclo,na.rm=TRUE)
  maxy <- max(dt_resumo$ul_total,na.rm=TRUE)
  t.leg <- c.leg <- c()
  for (t in length(tipos):1)
  {
    if (t==length(tipos))
    {
      plot (NA,
            xlab="Ciclos", ylab="Individuos",
            xlim=c(0,maxx), ylim=c(0,maxy))
      points(dt_historiatotal_populacao$ciclo,
            dt_historiatotal_populacao$total,
            col="#00000002",cex=0.01)
      cor <- c("#000000","#ffffff")
      for (c.aux in length(cor):1)
      {
        lines(dt_resumo$ciclo,
              dt_resumo$m_total,
              col=cor[c.aux],lwd=2*c.aux)
        lines(dt_resumo$ciclo,
              dt_resumo$ll_total,
              col=cor[c.aux],lwd=0.5*c.aux,lty=2)
        lines(dt_resumo$ciclo,
              dt_resumo$ul_total,
              col=cor[c.aux],lwd=0.5*c.aux,lty=2)
      }
      t.leg <- c(t.leg,"total")
      c.leg <- c(c.leg,cor[1])
    } else
    {
      corbase <- friendlycolor(cores[t])
      c <- which(names(dt_historiatotal_populacao)==tipos[t])
      points(dt_historiatotal_populacao$ciclo,
             as.numeric(unlist(dt_historiatotal_populacao[,c])),
             col=paste0(corbase,"02"),cex=0.01)
      c <- which(names(dt_resumo)==paste0("m_",tipos[t]))
      cor <- c(corbase,"#ffffff")
      for (c.aux in length(cor):1)
      {
        lines(dt_resumo$ciclo,
              as.numeric(unlist(dt_resumo[,c])),
              col=cor[c.aux],lwd=2*c.aux)
        lines(dt_resumo$ciclo,
              as.numeric(unlist(dt_resumo[,c+1])),
              col=cor[c.aux],lwd=0.5*c.aux,lty=2)
        lines(dt_resumo$ciclo,
              as.numeric(unlist(dt_resumo[,c+2])),
              col=cor[c.aux],lwd=0.5*c.aux,lty=2)
      }
      t.leg <- c(t.leg,tipos[t])
      c.leg <- c(c.leg,corbase)
    }
  }
  legend ("topleft",
          t.leg,
          lwd=1,
          lty=1,
          col=c.leg,
          box.lwd=0,
          bg="transparent")
  if (png)
  {
    dev.off()
  }
  
  
  # proporcao
  if (png)
  {
    png(paste0(filename,"_prc.png"), width=800, height=550)
  }
  maxx <- max(dt_resumo$ciclo,na.rm=TRUE)
  maxy <- 1.15
  t.leg <- c.leg <- c()
  for (t in (length(tipos)-1):1)
  {
    c <- which(names(dt_resumo)==paste0("m_",tipos[t],"_p"))
    if (t==(length(tipos)-1))
    {
      plot (NA,
            xlab="Ciclos", ylab="Proporcao",
            xlim=c(0,maxx), ylim=c(0,maxy))
    } 
    corbase <- friendlycolor(cores[t])
    lines(dt_resumo$ciclos,
          as.numeric(unlist(dt_resumo[,c])),
          col=corbase,lwd=2)
    lines(dt_resumo$ciclos,
          as.numeric(unlist(dt_resumo[,c+1])),
          col=corbase,lwd=0.5,lty=2)
    lines(dt_resumo$ciclos,
          as.numeric(unlist(dt_resumo[,c+2])),
          col=corbase,lwd=0.5,lty=2)
    t.leg <- c(t.leg,tipos[t])
    c.leg <- c(c.leg,corbase)
  }
  legend ("left",
          t.leg,
          lwd=1,
          lty=1,
          col=c.leg,
          box.lwd=0,
          bg="transparent")
  if (png)
  {
    dev.off()
  }
}
