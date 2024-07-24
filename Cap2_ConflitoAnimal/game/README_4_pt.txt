#################################################
# mygame 4.0                                    #
# (c) J.O.Siqueira, P.S.P.Silveira, 2018jun28   #
#     usando baryplot by Richard McElreath      #
#################################################

ATENÇÂO: os jogos anda precisam ser revisados


INSTRUÇÕES

Instalação:
----------

Abra um terminal R

Caso você ainda não o tenha feito, instale o pacote baryplot
executando as seguintes duas linhas:

options(repos=c(getOption("repos"),baryplot="http://xcelab.net/R"))
install.packages("baryplot",type="source")

Executando mygame:
------------------

Abra um terminal R

Carregue o programa com o fonte desejado:
  source("mygame.R") 

Use um dos jogos definidos ou crie uma nova definição
(veja Retaliator.txt para detalhes)
  
Execute com:
  mygame ("Nome_do_jogo", parametros, options="lista_de_opcoes")

ou use:
  mygame()
  (o sistema exibe a sintaxe)
  
Parâmetros:
  Você pode fornecer os parâmetros pré-definidos ou omití-los.
  Parâmetros adicionais que você tenha criado para algum jogo precisam ser 
  explicitamente declarados.

  São pré-definidos:
  lines: número de linhas (default = 100)
  v: valor do recurso (default = 2)
  c: custo para um Hawk ao perder a luta com outro Hawk (default = 3)
  w0: "baseline fitness" (default = 5) --- alterar este parâmetro afeta
      o número de passos (e, portanto, a velocidade) de plotagem de uma linha
  d: custo para display (default = 0)

Lista de opções:
  Alteram o layout e as regiões do diagrama explorados:
  Cor: color, gray, black ... use apenas um dos três modos (default = black)
  Setas: arrows, no_arrows ... use apenas uma dos dois modos (default = arrows)
  Tamanho da seta: arrow_size = # ... (default = 1); sem efeito com no_arrows
  Marcadores: dots, start_dots, end_dots, no_dots ... respectivamente,
              - dots: marcadores de início (vazado) e fim (sólido) de cada linha
              - start_dots: apenas o marcador do início da linha
              - end_dots: apenas o marcador do fim da linha
              - no_dots: sem marcadores (não usar em combinação com os outros)
  Áreas para explorar: all_areas, corners, edges, borders, inner_area ...
              - all_areas: default, todas as abaixo (não usar em combinação com os outros)
              - corners: início das linhas sobre os vértices
              - edges: início das linhas sobre as arestas, próximo aos vértices
              - borders: início das linhas na área interna, próximo às arestas
              - inner_area: início das linhas na área interna, longe da arestas
                (corners, edges, borders e inner_areas podem ser combinados)

  Apresentação: ternary, tridimensional
    Define o tipo de gráfico: 
              - ternary: plano (default)
              - tridimensional: 3D 

Alterações:

  2018may14: na busca de "edges" explora ao longo da aresta
 
Exemplos:

  mygame ("Retaliator")
    jogo definido em Retaliator.txt, com todos os defaults.

  mygame ("Retaliator", options="tridimensional")
    jogo definido em Retaliator.txt, visão 3D, com todos os defaults.

  mygame ("Retaliator", v=1, c=2, w0=10, d=0.5, options="color, arrows, arrow_size=1.5, end_dots, borders, inner_area")
    jogo definido em Retaliator.txt, computando display=0.5, plotado com cor, setas aumentadas de 50%, apenas os pontos finais, explorando a área interior com linhas iniciadas junto e afastadas das arestas.  

  mygame ("Retaliator", v=1, c=2, w0=10, d=0.5, options="color, edges")
    jogo definido em Retaliator.txt, computando display=0.5, plotado com cor, explorando apenas as arestas.  

