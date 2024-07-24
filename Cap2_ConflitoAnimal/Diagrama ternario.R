# options(repos=c(getOption("repos"),baryplot="http://xcelab.net/R"))
# install.packages("baryplot",type="source")

library("baryplot")
bary.init()
bary.labels("Hawk","Retaliator","Dove")
bary.plotsim(1/3, 1/3, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.click()

bary.game.hdr

# hdr: versao 1
options(repos=c(getOption("repos"),baryplot="http://xcelab.net/R"))
install.packages("baryplot",type="source")
library("baryplot")

bary.init()
bary.labels("Hawk","Retaliator","Dove")
x <- seq(0, 1, by = 0.1)
for (p in x)
{
	for (q in x)
	{
		k = 1 - p - q
		if (k >= 0)
		{
			bary.plotsim(p, q, thegame=bary.game.hdr, arrow=TRUE, withcol=TRUE, w=5)
		}
	}
}

# hdr: versao 2
bary.init()
bary.labels("Hawk","Retaliator","Dove")
valor1 <- runif(15,0,1)
valor1 <- c(0,1,valor1)
valor2 <- runif(15,0,1)
valor2 <- c(0,1,valor1)
for (p in valor1)
{
  for (q in valor2)
  {
    k = 1 - p - q
    if (k >= 0)
    {
      bary.plotsim(p, q, thegame=bary.game.hdr, arrow=TRUE, withcol=TRUE, w=5)
    }
  }
}

# hdr: versao 3
bary.init()
bary.labels("Hawk","Retaliator","Dove")
bary.plotsim(0, 0, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1, 0, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(0, 1, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(2/3, 0, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(0, 2/5, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1/100, 0, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(99/100, 0, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1/3, 1/3, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(8/10, 1/10, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1/10, 1/10, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(3/10, 5/10, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(4.5/10, 4.5/10, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(0, 0.3, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(0, 0.7, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(99/100, 1/100, thegame=bary.game.hdr, arrow=TRUE, withcol=FALSE, w=0.9)

# Burgues
bary.game.hdb <- function(p, q, r, v=2, c=3, w0 = 5) {
  w1 <- p * (v-c)/2 + q * (v/2+(v-c)/4) + r * v + w0
  w2 <- p * (v-c)/4 + q * v/2 + r * 3*v/4 + w0
  w3 <- q * v/4 + r * v/2 + w0
  c(w1,w2,w3)
}
bary.init()
bary.labels("Hawk","Bourgeois","Dove")

# Exploracao parcimoniosa
bary.plotsim(2/3, 1/100, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(0, 1/3, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(2/3, 1/3, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1/5, 1/5, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(1/10, 0, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(99/100, 0, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)
bary.plotsim(4/5, 1/6, thegame=bary.game.hdb, arrow=TRUE, withcol=FALSE, w=0.9)


# Exploracao aleatória
valor1 <- runif(15,0,1)
valor1 <- c(0,1,valor1)
valor2 <- runif(15,0,1)
valor2 <- c(0,1,valor1)
for (p in valor1)
{
  for (q in valor2)
  {
    k = 1 - p - q
    if (k >= 0)
    {
      bary.plotsim(p, q, thegame=bary.game.hdb, arrow=TRUE, withcol=TRUE, w=5)
    }
  }
}


# assessor: v = 3, c = 2, x = 0.75 > 3/5 = 0.6 
assessor <- function (p, q, r, v = 3, c = 2, w0 = 5) 
{
		x <- 0.55
    w1 <- p * (v - c)/2 + q/2*(v+((1-x)*v-x*c)) + (1-p-q)*v + w0
    w2 <- p/2 * (x*v -(1-x)*c) + q*v/2 + (1-p-q)*(3*v/4) + w0
    w3 <- q * v / 4 + (1-p-q) * v/2 + w0
    c(w1, w2, w3)
}
bary.init()
bary.labels("Hawk","Assessor","Dove")
valor1 <- runif(15,0,1)
valor1 <- c(0,1,valor1)
valor2 <- runif(15,0,1)
valor2 <- c(0,1,valor1)
for (p in valor1)
{
  for (q in valor2)
  {
    k = 1 - p - q
    if (k >= 0)
    {
      bary.plotsim(p, q, thegame=assessor, arrow=TRUE, withcol=TRUE, w=5)
    }
  }
}

# assessor: v = 2, c = 3, v < c, qualquer x > 0.5
assessor <- function (p, q, r, v = 2, c = 3, w0 = 5) 
{
  x <- 0.75
  w1 <- p * (v - c)/2 + q/2*(v+((1-x)*v-x*c)) + (1-p-q)*v + w0
  w2 <- p/2 * (x*v -(1-x)*c) + q*v/2 + (1-p-q)*(3*v/4) + w0
  w3 <- q * v / 4 + (1-p-q) * v/2 + w0
  c(w1, w2, w3)
}
bary.init()
bary.labels("Hawk","Assessor","Dove")
valor1 <- runif(15,0,1)
valor1 <- c(0,1,valor1)
valor2 <- runif(15,0,1)
valor2 <- c(0,1,valor1)
for (p in valor1)
{
  for (q in valor2)
  {
    k = 1 - p - q
    if (k >= 0)
    {
      bary.plotsim(p, q, thegame=assessor, arrow=TRUE, withcol=TRUE, w=5)
    }
  }
}

# assessor: v = 3, c = 2, v > c, 0.5 < x = 0.55 < 0.6
assessor <- function (p, q, r, v = 3, c = 2, w0 = 5) 
{
  x <- 0.55
  w1 <- p * (v - c)/2 + q/2*(v+((1-x)*v-x*c)) + (1-p-q)*v + w0
  w2 <- p/2 * (x*v -(1-x)*c) + q*v/2 + (1-p-q)*(3*v/4) + w0
  w3 <- q * v / 4 + (1-p-q) * v/2 + w0
  c(w1, w2, w3)
}
bary.init()
bary.labels("Hawk","Assessor","Dove")
valor1 <- runif(15,0,1)
valor1 <- c(0,1,valor1)
valor2 <- runif(15,0,1)
valor2 <- c(0,1,valor1)
for (p in valor1)
{
  for (q in valor2)
  {
    k = 1 - p - q
    if (k >= 0)
    {
      bary.plotsim(p, q, thegame=assessor, arrow=TRUE, withcol=TRUE, w=5)
    }
  }
}
