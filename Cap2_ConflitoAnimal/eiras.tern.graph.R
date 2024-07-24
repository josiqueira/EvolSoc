source ("eiras.tern.coord.R")

eiras.tern.graph <- function (
  # title
  main = "",
  # vertex labels
  label.left = "A",
  label.right = "B",
  label.top = "C",
  label.size = 1,
  # vertex colors
  v.col = "#000000",
  v.bg = "#ffffff",
  # graph background color
  g.bg = "#d8d8d8",
  # graph ticklines color
  g.tl = "#ffffff"
)
{
  # original margins
  o.par <- par(mar = rep(0.9,4))
  # triangle coordinates
  # lv        rv        tv
  # A:(0,0,1) B:(0,1,0) C:(1,0,0)
  coord <- eiras.tern.coord(c(0,0,1,0,1,0,1,0,0))
  # left vertex (lv)
  lv.x <- coord[1,1]
  lv.y <- coord[1,2]
  # right vertex (rv)
  rv.x <- coord[2,1]
  rv.y <- coord[2,2]
  # top vertex (tv)
  tv.x <- coord[3,1]
  tv.y <- coord[3,2]
  
  margin <- 0.08
  plot(NA, 
       main=main,
       xlim=c(0-margin,max(lv.x,tv.x,rv.x)+margin), 
       ylim=c(0-margin,max(lv.y,tv.y,rv.y)+margin) 
       , axes=FALSE, xlab="", ylab=""
  )
  
  # if there is no top vertex, game with two strategies
  num.strategies <- 3
  if(is.na(label.top)==TRUE)
  {
    num.strategies <- 2
  }
  
  if (num.strategies==2)
  {
    # axes: rltr
    lines(c(rv.x,lv.x),
          c(rv.y,lv.y),
          lty=2, col="#444444", lwd=0.6)
    # draw vertices
    points(c(rv.x,lv.x),
           c(rv.y,lv.y),
           pch=21, col=v.col, bg=v.bg)
    # vertex label coordinates
    text(lv.x-margin/3,lv.y-margin/3, srt=-30, cex=label.size, label.left)
    text(rv.x+margin/3,rv.y-margin/3, srt= 30, cex=label.size, label.right)
  } # num.strategies==2
  if (num.strategies==3)
  {
    # background
    polygon(c(rv.x,lv.x,tv.x,rv.x),
            c(rv.y,lv.y,tv.y,rv.y),
            col=g.bg)
    # horizontal tick lines
    # lv        rv        tv
    # A:(0,0,1) B:(0,1,0) C:(1,0,0)
    # A:(0,0,1) B:(0,1,0)           ... left-right axis
    steps <- seq(0,1,0.1)
    lra <- c()
    for(s in steps)
    {
      lra <- c(lra, 0, s, 1-s)
    }
    lra <- eiras.tern.coord(lra)
    #           B:(0,1,0) C:(1,0,0) ... right-top axis
    rta <- c()
    for(s in steps)
    {
      rta <- c(rta, 1-s, s, 0)
    }
    rta <- eiras.tern.coord(rta)
    # A:(0,0,1)           C:(1,0,0) ... left-top axis
    lta <- c()
    for(s in steps)
    {
      lta <- c(lta, s, 0, 1-s)
    }
    lta <- eiras.tern.coord(lta)
    # tick lines
    steps <- 1:11
    for(s in steps)
    {
      # paralell to lra
      lines(c(lta[s,1],rta[12-s,1]),c(lta[s,2],rta[12-s,2]),col=g.tl)
      # paralell to lta
      lines(c(lra[s,1],rta[   s,1]),c(lra[s,2],rta[   s,2]),col=g.tl)
      # paralell to rta
      lines(c(lra[s,1],lta[   s,1]),c(lra[s,2],lta[   s,2]),col=g.tl)
    }
    # axes: rltr
    lines(c(rv.x,lv.x,tv.x,rv.x),
          c(rv.y,lv.y,tv.y,rv.y),
          lty=2, col="#444444", lwd=0.6)
    # draw vertices
    points(c(rv.x,lv.x,tv.x),
           c(rv.y,lv.y,tv.y),
           pch=21, col=v.col, bg=v.bg)
    # vertex label coordinates
    text(lv.x-margin/3,lv.y-margin/3, srt=-30, cex=label.size, label.left)
    text(rv.x+margin/3,rv.y-margin/3, srt= 30, cex=label.size, label.right)
    text(tv.x         ,tv.y+margin/2, srt=  0, cex=label.size, label.top)
  } # num.strategies==3
  
  
  # restore original margins
  par(o.par)
}


