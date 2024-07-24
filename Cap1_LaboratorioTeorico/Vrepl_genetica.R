# Vrepl_genetica.R

V <- function (genotype)
{
  fitness <- 0
  if(genotype=="A") {fitness<-0.8}
  if(genotype=="B") {fitness<-0.5}
  return (fitness)
}
