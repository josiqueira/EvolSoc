# Silveira's implemented
friendlycolor <- function(idxs)
{
  fcor <- c()

  # 1
  fcor <- c(fcor,"#882E72") # violeta escuro
  # 2
  fcor <- c(fcor,"#994F88") # violeta 5
  # 3
  fcor <- c(fcor,"#AA6F9E") # violeta 4
  # 4
  fcor <- c(fcor,"#BA8DB4") # violeta 3
  # 5
  fcor <- c(fcor,"#CAACCB") # violeta 2
  # 6
  fcor <- c(fcor,"#D9CCE3") # violeta claro
  # 7
  fcor <- c(fcor,"#0d5092") # azul naval
  # 8
  fcor <- c(fcor,"#1965B0") # azul cobalto
  # 9
  fcor <- c(fcor,"#437DBF") # azul 2
  # 10
  fcor <- c(fcor,"#6195CF") # azul 3
  # 11
  fcor <- c(fcor,"#7BAFDE") # azul celeste
  # 12
  fcor <- c(fcor,"#a0d8d8") # azul claro
  # 13
  fcor <- c(fcor,"#507052") # verde musgo
  # 14
  fcor <- c(fcor,"#26a169") # verde médio
  # 15
  fcor <- c(fcor,"#4EB265") # verde folha
  # 16
  fcor <- c(fcor,"#90C987") # verde claro 1
  # 17
  fcor <- c(fcor,"#a6da9a") # verde claro 2
  # 18
  fcor <- c(fcor,"#CAE0AB") # verde claro 3
  # 19
  fcor <- c(fcor,"#ac4d12") # ocre
  # 20
  fcor <- c(fcor,"#E65518") # laranja escuro
  # 21
  fcor <- c(fcor,"#EE8026") # laranja 3
  # 22
  fcor <- c(fcor,"#F4A736") # laranja 2
  # 23
  fcor <- c(fcor,"#F7CB45") # laranja claro
  # 24
  fcor <- c(fcor,"#F7F056") # amarelo
  # 25
  fcor <- c(fcor,"#42150A") # marrom escuro
  # 26
  fcor <- c(fcor,"#72190E") # marrom médio
  # 27
  fcor <- c(fcor,"#A5170E") # marrom claro
  # 28
  fcor <- c(fcor,"#a30b1b") # bordô
  # 29
  fcor <- c(fcor,"#a3261b") # terra
  # 30
  fcor <- c(fcor,"#f43328") # tijolo
  # 31
  fcor <- c(fcor,"#000000") # preto
  # 32
  fcor <- c(fcor,"#111111") # cinza 1
  # 33
  fcor <- c(fcor,"#222222") # cinza 2
  # 34
  fcor <- c(fcor,"#333333") # cinza 3
  # 35
  fcor <- c(fcor,"#444444") # cinza 4
  # 36
  fcor <- c(fcor,"#555555") # cinza 5
  # 37
  fcor <- c(fcor,"#666666") # cinza 6
  # 38
  fcor <- c(fcor,"#777777") # cinza 7
  # 39
  fcor <- c(fcor,"#888888") # cinza 8
  # 40
  fcor <- c(fcor,"#999999") # cinza 9
  # 41
  fcor <- c(fcor,"#aaaaaa") # cinza 10
  # 42
  fcor <- c(fcor,"#bbbbbb") # cinza 11
  # 43
  fcor <- c(fcor,"#cccccc") # cinza 12
  # 44
  fcor <- c(fcor,"#dddddd") # cinza 13
  # 45
  fcor <- c(fcor,"#eeeeee") # cinza 14
  # 46
  fcor <- c(fcor,"#ffffff") # branco

  retorno <- c()
  for (idx in 1:length(idxs))
  {
    retorno <- c(retorno, fcor[idxs[idx]])
  }
  
  return (retorno)
}

