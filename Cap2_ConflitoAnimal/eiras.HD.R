# eiras.HD.R

W <- function (type, p, w0, v, c)
{
  VHH <- (v-c)/2
  VHD <- v
  VDH <- 0
  VDD <- v/2
  if (type=="H")
  {
    fitness <- w0+p*VHH+(1-p)*VHD
  }
  if (type=="D")
  {
    fitness <- w0+p*VDH+(1-p)*VDD
  }
  return (fitness)
}
