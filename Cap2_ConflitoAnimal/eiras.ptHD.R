# eiras.ptHD.R

source("eiras.HD.R")
source("eiras.friendlycolor.R")

ptHD <- function(pH,pD,w0,v,c,t)
{
  pH <- pH/(pH+pD)
  pD <- 1-pH
  t <- seq(1,t,1)
  plot(NA,
       type="p", 
       main=paste0("Evolucao da proporcao de Hawk & Dove\n",
                   "pH_0 = ",pH,", w0 = ",w0,", v = ",v,", c = ",c), 
       xlim=c(0,max(t)),ylim=c(0,1),
       ylab="p(t)", xlab="t",col="black",bg="white")
  corH <- friendlycolor(30)
  corD <- friendlycolor(9)
  legend ("left",	
          c("H","D"),
          lwd=1,	
          lty=1,	
          col=c(corH,corD),	
          box.lwd=0,	
          bg="transparent")	
  # evolucao no tempo
  c_pH <- c_pD <- c()
  for (t.aux in t)
  {
    c_pH <- c(c_pH,pH)
    c_pD <- c(c_pD,pD)
    oldpH <- pH
    oldpD <- pD
    WH <- W("H",pH,w0,v,c)
    WD <- W("D",pH,w0,v,c)
    w <- pH*WD+pD*WD
    deltapH <- pH*(1-pH)*(WH-WD)/w
    pH <- pH+deltapH
    pD <- 1-pH
    lines(c(t.aux-1,t.aux), c(oldpH,pH), col=corH)
    points(t.aux,pH,pch=21,col=corH,bg="white")
    lines(c(t.aux-1,t.aux), c(oldpD,pD), col=corD)
    points(t.aux,pD,pch=21,col=corD,bg="white")
  }
  idx <- round(length(c_pH)/10,0)
  m_pH <- mean(c_pH[(length(c_pH)-idx):length(c_pH)])
  abline(h=m_pH,lty=2,col=corH)
  text(length(c_pH),m_pH,round(m_pH,3),cex=0.7,pos=1)
  m_pD <- mean(c_pD[(length(c_pD)-idx):length(c_pD)])
  abline(h=m_pD,lty=2,col=corD)
  text(length(c_pD),m_pD,round(m_pD,3),cex=0.7,pos=3)
}

