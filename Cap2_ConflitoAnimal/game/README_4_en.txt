#################################################
# mygame 4.0                                    #
# (c) J.O.Siqueira, P.S.P.Silveira, 2018jul01   #
#     applying baryplot by Richard McElreath    #
#################################################

INSTRUCTIONS

Installation:
-------------

Open a R terminal 

If you still does not have it, please install baryplot package
by executing these two lines:

options(repos=c(getOption("repos"),baryplot="http://xcelab.net/R"))
install.packages("baryplot",type="source")

Using mygame:
-------------

Open a R terminal 

Load mygame:
  source("mygame.R")

Apply one defined game or make a new definition
(see Retaliator.txt for exemple)
  
Use:
  mygame()
  to see sintax

Execute with:
  mygame ("Game_name")
    to apply all default conditions
  mygame ("Game_name", parameters, options="list_of_options")
    to apply conditions you want to test

Parameters:
  You may supply predefined parameters or supply any value to them.
  Additional parameters can be created to any game, but all of them must be declared.

  São pré-definidos:
  lines: número de linhas (default = 100)
  w0: "baseline fitness" --- by changing this parameters
      the number os steps (thus, plotting speed) are affected

Option list:
  Color: color, gray, black ... use only one of them (default = black)
  Setas: arrows, no_arrows ... use only one of them (default = arrows)
  Arrow size: arrow_size = integer ... (default = 1); no effect with no_arrows
  Marks: dots, start_dots, end_dots, no_dots ... respectively,
              - dots: empty and solid circles for beginning and end of each line
              - start_dots: apply only empty, start circles
              - end_dots: apply only end, solid circles
              - no_dots: no marks
  Areas to explore: all_areas, corners, edges, borders, inner_area ...
              - all_areas: default, all below (do not combine with other area options)
              or combine one or more of:
              - corners: starting lines on corners
              - edges: starting lines on edges
              - borders: starting lines in the inner area, close to the edges
              - inner_area: starting lines in the inner area, away of the edges

  Presentation: ternary, tridimensional
    It defines the kind of graph: 
              - ternary: flat (default)
              - tridimensional: 3D view

Examples:

  mygame ("Retaliator")
    game with all defaults.

  mygame ("Retaliator", options="tridimensional")
    3D views, all other defaults.

  mygame ("Retaliator", v=1, c=2, w0=10, d=0.5, options="color, arrows, arrow_size=1.5, end_dots, borders, inner_area")
    cost for display=0.5, plotting with colors, arrow sizes increased by 50%, showing only end dots, 
    exploring the inner area with lines starting close to and away of the edges

