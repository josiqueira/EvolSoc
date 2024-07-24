# eiras.col2rgbstring.R

library(grDevices)
source("eiras.text.leading.R")

eiras.col2rgbstring <- function(col, alpha=FALSE)
{
  rgb <- col2rgb(col,alpha)
  r <- eiras.text.leading(as.character(as.hexmode(as.numeric(rgb[1]))),2,"0")
  g <- eiras.text.leading(as.character(as.hexmode(as.numeric(rgb[2]))),2,"0")
  b <- eiras.text.leading(as.character(as.hexmode(as.numeric(rgb[3]))),2,"0")
  a <- ""
  if (alpha==TRUE)
  {
    a <- eiras.text.leading(as.hexmode(as.numeric(rgb[4])),2,"0")
  }
  return(paste("#",r,g,b,a,sep=""))
}