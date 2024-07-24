# eiras.rgb2rgbstring.R

library(grDevices)
source("eiras.text.leading.R")

rgb2rgbstring <- function(r, g, b, a)
{
  r <- round(r,0)
  g <- round(g,0)
  b <- round(b,0)
  a <- try(round(a,0), silent = TRUE)
  r <- text.leading(as.character(as.hexmode(as.numeric(r))),2,"0")
  g <- text.leading(as.character(as.hexmode(as.numeric(g))),2,"0")
  b <- text.leading(as.character(as.hexmode(as.numeric(b))),2,"0")
  if (is.numeric(a))
  {
    a <- text.leading(as.character(as.hexmode(as.numeric(a))),2,"0")
  } else
  {
    a <- ""
  }
  return(paste("#",r,g,b,a,sep=""))
}