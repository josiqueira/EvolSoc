# eiras.redblue.gradation.R
# receives a hot[0,1] level and returns rgb

source("eiras.rgb2rgbstring.R")

eiras.redblue.gradation <- function (hot, color=TRUE)
{
  # hot ... r
  #  1  ... 255
  if (hot<0) {hot <- 0}
  if (hot>1) {hot <- 1}
  if (color==TRUE)
  {
    r <- 255*hot
    if (r<(255/2))
    {
      g <- r
    } else
    {
      g <- 150-r/2.5
    }
    b <- 170-r/1.5 # (256/4)-r/4

    # # g <- r/4
    # # b <- 180-r/2
    # if (r<(255/2))
    # {
    #   g <- r
    # } else
    # {
    #   g <- 255/1.25-r/3
    # }
    # b <- 205-r/1.25 # (256/4)-r/4
    
  } else
  {
    r <- 10+100*hot
    g <- b <- r
  }
  return (rgb2rgbstring(r,g,b))
}
# 
# plot(NA,xlim=c(0,1),ylim=c(0,1),
#      xlab="red",ylab="green"
# )
# for (h in seq(0,1,0.01))
# {
#   col<-eiras.redblue.gradation(h)
#   points(h,0.5,pch=21,col=col,bg=col)
# 
# }

# plot(NA,xlim=c(0,255),ylim=c(0,255),
#      xlab="red",ylab="green"
# )
# for (r in seq(0,255,2))
# {
#   x <- r
#   if (r<(255/2))
#   {
#     g <- r
#   } else
#   {
#     g <- 150-r/2.5
#   }
#   b <- 170-r/1.5 # (256/4)-r/4
#   y <- g
#   cat(r,g,b,"\n")
#   col <- eiras.rgb2rgbstring(r,g,b)
#   points(x,y,
#          cex=1,pch=21,bg=col,col=col)
# }
