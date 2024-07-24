library(baryplot)
source("R/mygame.R")
banana.game <- function (p, q, r, w0 = 5, v = 2, c = 3, xp = 0.45, ...) 
{
  # hawk
  w1 <- w0 + p*((v-c)/2) + q*((1/2)*v + (1/2)*((1-xp)*v-xp*c)) + r * v
  # banana
  w2 <- w0 + p*((1/2)*(xp*v - (1-xp)*c)) + q*((1/2)*v) + r*((1/2)*v + (1/2)*(v/2))
  # dove
  w3 <- w0 + p*(0) + q*((1/2)*(v/2)) + r*(v/2)
  c(w1, w2, w3)
}
mygame("R/Banana", lines=20, options="black")
baryplot::bary.plotsim(4/7, 8/35, 
                       thegame=banana.game,
                       arrow=TRUE)
mygame("R/Banana", lines=20, options="black")
baryplot::bary.plotsim(4/7+1/100, 8/35, 
                       thegame=banana.game,
                       arrow=TRUE, withcol=TRUE)
mygame("R/Banana", lines=20, options="black")
baryplot::bary.plotsim(4/7, 8/35+1/100, 
                       thegame=banana.game,
                       arrow=TRUE, withcol=TRUE)
mygame("R/Banana", lines=20, options="black")
baryplot::bary.plotsim(4/7-1/100, 8/35, 
                       thegame=banana.game,
                       arrow=TRUE, withcol=TRUE)
mygame("Banana", lines=20, options="black")
baryplot::bary.plotsim(4/7, 8/35-1/100, 
                       thegame=banana.game,
                       arrow=TRUE, withcol=TRUE)
