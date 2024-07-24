# eiras.V.R

V <- function (maxfit, p)
{
  # 1-exp(-1) --------- maxfit
  # exp(-p)-exp(-1)+0.001 --- V 
  fitness <- (maxfit*(exp(-p)-exp(-1)))/(1-exp(-1))
  return (fitness)
}


