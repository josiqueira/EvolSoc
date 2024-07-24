# eiras.tern.coord.R

eiras.tern.coord <- function (triade)
{
  pairs <- length(triade)/3
  r <- matrix(data=NA, ncol=5, nrow=pairs)
  # convert to percentage
  rlin <- 0
  for (t in seq(1,length(triade),3))
  {
    rlin <- rlin+1
    totlin <- sum(triade[t:(t+2)],na.rm=TRUE)
    r[rlin,3] <- triade[t  ]/totlin
    r[rlin,4] <- triade[t+1]/totlin
    r[rlin,5] <- triade[t+2]/totlin
    # https://mathworld.wolfram.com/TernaryDiagram.html
    # x
    r[rlin,1] <- ( (r[rlin,3]+2*r[rlin,4])/(r[rlin,3]+r[rlin,4]+r[rlin,5]) ) / 2
    # y
    r[rlin,2] <- (  r[rlin,3]     /(r[rlin,3]+r[rlin,4]+r[rlin,5]) ) / ((3^0.5)/2)
  }

  return(r)
}