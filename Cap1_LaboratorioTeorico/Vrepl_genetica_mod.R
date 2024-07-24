# Vrepl_genetica.R

V <- function (d, p)
{
  fitness <- exp(-d*p)
  return (fitness)
}
