version <- "
#################################################
# mygame v 4.01                                 #
#                                               #
# (c) J.O.Siqueira, P.S.P.Silveira, 2018 Jul 03 #
#     applying baryplot by Richard McElreath    #
#################################################
";

# options(repos=c(getOption("repos"),baryplot="http://xcelab.net/R"))
# install.packages("baryplot",type="source")

library(baryplot)
  
# color="n", arrow="y", explore="all") 
mygame <- function(game_name, lines=100, w0=5, options="", ...)
{
  cat ("\n",version,"\n");
  
  ap <- c()
  aq <- c()

  menserro <- ""
  warning <- ""
  ok <- 1
  if (missing(game_name))
  {
    ok <- 0
    menserro <- c(menserro,"\tMissing game name (see \"Sintax\", above).")
  }
  
  if (ok == 1)
  {
    # Escolha do jogo 
    ok <- 0; 
    game_name <- gsub(".txt","",game_name);
    game_name = paste (game_name,".txt",sep="");
    source (game_name);
    dados <- ps.config();
    ok <- numcorners <- dados[1];
    label_left <- dados[2];
    label_right <- dados[3];
    label_top <- dados[4];
    run_game <- ps.game;
#                                         cat (game_name,numcorners,ok,"\n");
    
    if (ok == 0)
    {
      menserro <- c(menserro,"\tPlease choose a valid game name.");
    }
    
    if (lines <= 0)
    {
      menserro <- "\tLines must be a positive number.";
    }
    
    # options (default)
    color <- ""
    arrow <- ""
    dot <- ""
      startdot <- ""
      enddot <- ""
    explore <- ""
      inner <- "n"
      border <- "n"
      edge <- "n"
      corner <- "n"
      kindgraph = "ternary"
    arrowvalue <- 0
#                             cat ("options",length(options),"\n");
                        
    options = strsplit(options,"[ ,]")[[1]]
    options = unique(tolower(options))
    options = c(options,"#end#")
    l_idx = 1;
    while (options[l_idx] != "#end#")
    {
      found = 0
      
      options[l_idx] = gsub("^\\s+|\\s+$", "", options[l_idx])
      if (options[l_idx] != "")
      {  
        if (options[l_idx] == "nocolor" || options[l_idx] == "nocolors" || 
            options[l_idx] == "no_color" || options[l_idx] == "no_colors" || 
            options[l_idx] == "black")
        {
          color = "n"
          found = 1
        }
        if (options[l_idx] == "gray")
        {
          color = "g"
          found = 1
        }
        if (options[l_idx] == "color" || options[l_idx] == "colors")
        {
          color = "y"
          found = 1
        }
        
        if (options[l_idx] == "arrow" || options[l_idx] == "arrows")
        {
          arrow = "y"
          found = 1
        }
        if (options[l_idx] == "noarrow" || options[l_idx] == "noarrows" || 
            options[l_idx] == "no_arrow" || options[l_idx] == "no_arrows")
        {
          arrow = "n"
          found = 1
        }
    #                                         xxx = length(label_top); cat("len ",xxx,"\n")
        if (options[l_idx] == "dot" || options[l_idx] == "dots")
        {
          dot = "y"
          startdot = "y"
          enddot = "y"
          found = 1
        }
        if (options[l_idx] == "nodot" || options[l_idx] == "nodots" || 
            options[l_idx] == "no_dot" || options[l_idx] == "no_dots")
        {
          dot = "n"
          startdot = "n"
          enddot = "n"
          found = 1
        }
        if (options[l_idx] == "startdot" || options[l_idx] == "startdots" || 
            options[l_idx] == "start_dot" || options[l_idx] == "start_dots")
        {
          dot = "y"
          startdot = "y"
          enddot = "n"
          found = 1
        }
        if (options[l_idx] == "enddot" || options[l_idx] == "enddots" || 
            options[l_idx] == "end_dot" || options[l_idx] == "end_dots")
        {
          dot = "y"
          startdot = "n"
          enddot = "y"
          found = 1
        }
        
        # all|corners|edges|borders
        if (options[l_idx] == "allarea" || options[l_idx] == "allareas" || 
            options[l_idx] == "all_area" || options[l_idx] == "all_areas")
        {
          explore = "all"
          inner <- "y"
          border <- "y"
          edge <- "y"
          corner <- "y"
          found = 1
        }
        if (options[l_idx] == "innerarea" || options[l_idx] == "innerareas" || 
            options[l_idx] == "inner_area" || options[l_idx] == "inner_areas")
        {
          explore = "partial"
          inner <- "y"
          found = 1
        }
        if (options[l_idx] == "border" || options[l_idx] == "borders")
        {
          explore = "partial"
          border <- "y"
          found = 1
        }
        if (options[l_idx] == "edge" || options[l_idx] == "edges")
        {
          explore = "partial"
          edge <- "y"
          found = 1
        }
        if (options[l_idx] == "corner" || options[l_idx] == "corners")
        {
          explore = "partial"
          corner <- "y"
          found = 1
        }

        if (options[l_idx] == "ternary")
        {
          kindgraph = "ternary"
          found = 1
        }
        
        # tamanho das setas
        if  (
              substring(options[l_idx],1,9) == "arrowsize" ||
              substring(options[l_idx],1,10) == "arrow_size"
            )
        {
          pos <- regexpr('=', options[l_idx])
          arrowvalue = as.numeric(substring(options[l_idx],pos+1,100))
#                                                cat ("\nEncontra ",arrowvalue ,pos,"\n") 
          found = 1
        }
        
        if (options[l_idx] == "tridimensional")
        {
          kindgraph = "tridimensional"
          arrow = "n" # por enquanto nao ploto setas no 3D
          found = 1
        }
      
        # parametro nao existe
        if (found == 0)
        {
          menserro <- c(menserro,paste("\tUnknown option:",options[l_idx]));
        }
      }
      
      l_idx = l_idx+1;
    }

    if (numcorners == 2)
    {
      arrow = "y"
      explore = "partial"
      corner = "n"
      inner <- "n"
      border <- "n"
      edge <- "y"
      warning <- c(warning,"\tGames with only two nodes: 
      \t- arrows are turned on,
      \t- areas and borders are not available,
      \t- only edges can be explored.");
    }

    
    # check color parameter
    find <- 0
    if (color=="y" || color=="g")
    {
      paramcolor = TRUE
      find <- 1
    }
    if (color=="n")
    {
      paramcolor = FALSE
      find <- 1
    }
    if (find==0)
    {
      warning <- c(warning,"\tLines in black by default")
      color = "n"
      paramcolor = FALSE
    }

  # check arrow parameter
    find <- 0
    if (arrow=="y")
    {
      paramarrow = TRUE
      find <- 1
    }
    if (arrow=="n")
    {
      paramarrow = FALSE
      find <- 1
    }
    if (find==0)
    {
      warning <- c(warning,"\tPlotting arrows by default")
      arrow = "y"
      paramarrow = TRUE
    }
#                                             cat ("arrowvalue=",arrowvalue,arrow,paramarrow,"\n")
    if (paramarrow)
    {
      find <- 1
      if (arrowvalue < 0.0)
      {
        menserro <- c(menserro,"\tArrow size must be greater than 0");
        find <- 0
      }
      else
      {
        if (arrowvalue == 0.0)
        {
          warning <- c(warning,"\tArrow size default: 1")
          arrowvalue = 1;
        }
      }
    }
    else
    {
      if (arrowvalue > 0.0)
      {
        warning <- c(warning,paste("\tNo arrows to plot: arrow size =",arrowvalue,"was dismissed."));
        arrowvalue = 1;
      }
    }

    # check dot parameter
    find <- 0
    if (dot=="y")
    {
      if (startdot == "y" && enddot == "y")
        paramdot = 'y'
      if (startdot == "y" && enddot == "n")
        paramdot = 's'
      if (startdot == "n" && enddot == "y")
        paramdot = 'e'
      find <- 1
    }
    if (dot=="n")
    {
      paramdot = "n"
      find <- 1
    }
    if (find==0)
    {
      warning <- c(warning,"\tPlotting start and end dots by default")
      dot = "y"
      paramdot = 'y'
    }
    
    # check explore parameter
    find <- 0
    if (explore=="all")
    {
      find <- 1
    }
    if (explore=="partial")
    {
      find <- 1
    }
    if (find==0)
    {
      warning <- c(warning,"\tExploring all_areas by default: all_areas")
      explore <- "all"
      inner <- "y"
      border <- "y"
      edge <- "y"
      corner <- "y"
    }

      # check kind of graph parameter
    find <- 0
    if (kindgraph=="ternary")
    {
      find <- 1
    }
    if (kindgraph=="tridimensional")
    {
      find <- 1
    }
    if (find==0)
    {
      warning <- c(warning,"\tTernary graph by default: ternary")
      kindgraph="ternary"
    }
    
    # check it there is any error
    menserro <- c(menserro,"#end#")
    m_idx = 1
    ok <- 1
    while (menserro[m_idx] != "#end#")
    {
      if (menserro[m_idx] != "")
      {
        ok <- 0
      }
      m_idx = m_idx+1
    }
  } 

  if (ok == 0) # faltando algo
  {
    cat ("\nSintax:\n");
    cat ("\tmygame (game_name, lines, w0, {more}, options=\"list\") \n");
    cat ("\tgame_name (a filename with any game definition (see \"Hawk & Dove.txt\")\n");
    cat ("\tlines: any value greater than 0 (default = 100)\n");
    cat ("\tw0: baseline fitness (default = 5)\n");
    cat ("\tmore: optional other variables, depending on game definition\n");
    cat ("\toptions=\"color|gray|black,
         \tarrows|no_arrows,
         \tarrow_size=#.##,
         \tdots|start_dot|end_dot|no_dots,
         \tall_areas|corners|edges|borders|inner_area,
         \tternary|tridimensional\"\n"); 
    cat ("\tExample: 
    \tmygame(\"Assessor\",lines=150,xp=0.7,options=\"gray arrow_size=1.2\")
    \t\t... It runs assessor game, with game-specific variable xp
    \t\t    drawing 150 lines in gray with arrows increased by 20% 
    \t\t    (see README_4_en.txt for details).
    \n");

    # show errors
    m_idx = 2
    stx = 0
    menserro <- c(menserro,"#end#")
    while (menserro[m_idx] != "#end#")
    {
      if (stx == 0)
      {
        cat ("\n\nErrors:");
        stx = 1
      }
      if (menserro[m_idx] != "")
      {
        cat ("\n",menserro[m_idx]);
      }
      m_idx = m_idx+1
    }
    cat ("\n\n");
    
  }
  else # tudo ok, executar
  {
    ps.bary.init(kindgraph=kindgraph, numcorners=numcorners);
    ps.showcolors(kindcolor=color, kindgraph=kindgraph);

    # Rotulos 
    if (kindgraph == "ternary")
    {
      bary.labels(label_right,label_top,label_left)
    }
    
    if (kindgraph == "tridimensional")
    {
      text (-0.1,0.5,label_left)
      text (1.5,-0.1,label_right)
      text (1,1.6,label_top)
    }

    cat ("-------------------------------------\n")
    cat ("Game:",game_name,"\n")
    cat ("\tlines=",lines,"\n")
    cat ("\tw0=",w0,"\n")
    cat ("\tcolors=",color,"\n")
    cat ("\tarrows=",arrow,"\n")
      if (arrow == "y")
      cat ("\tarrow size=",arrowvalue,"\n")
    cat ("\tdots=",dot," (start:",startdot,", end:",enddot,"),\n")
    cat ("\tareas=",explore,"\n")
    cat ("\t\tinner area=",inner,"\n")
    cat ("\t\tborders=",border,"\n")
    cat ("\t\tedges=",edge,"\n")
    cat ("\t\tcorners=",corner,"\n")
    # check it there is any warning
    warning <- c(warning,"#end#")
    m_idx = 2
    stx = 0
    while (warning[m_idx] != "#end#")
    {
      if (stx == 0)
      {
        cat ("\n\nWarning:");
        stx = 1
      }
      if (warning[m_idx] != "")
      {
        cat ("\n",warning[m_idx]);
      }
      m_idx = m_idx+1
    }
    cat ("\n\nDo not define variables names with reserved names: p, q, r or w0\n");
    # show errors
    m_idx = 2
    stx = 0
    menserro <- c(menserro,"#end#")
    while (menserro[m_idx] != "#end#")
    {
      if (stx == 0)
      {
        cat ("\n\nErrors:");
        stx = 1
      }
      if (menserro[m_idx] != "")
      {
        cat ("\n",menserro[m_idx]);
      }
      m_idx = m_idx+1
    }
    cat ("\n\n");
    
    comment <- ps.comment();
    cat ("\n************** COMMENTS **************\n",comment,"\n**************************************\n");
    
    # starting_points de partida
    value1 <- c()
    value2 <- c() 
    closemin <- 0.01
    closemax <- 0.99

    temparticao = 0
    if (explore=="all")
    {
      particao <- c(0, 0.05, 0.10, 0.15, 0.30, 0.45, 0.60)
      temparticao = 1
    }
    if (temparticao==0 && explore=="partial")
    {
      if (border == "y" && inner == "y")
      {
        # igual a explore=="all"
        particao <- c(0, 0.05, 0.10, 0.15, 0.30, 0.45, 0.60)
        temparticao = 1
      }
      if (temparticao==0 && border == "y")
      {
        particao <- c(0, 0.05, 0.10, 0.15, 0.43, 0.72, 1.00)
        temparticao = 1
      }
      if (temparticao==0 && border == "n" && inner == "y")
      {
        particao <- c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0)
        temparticao = 1
      }
    }
    starting_points <- 0
    if (temparticao == 1)
    {
      # Defina quantos starting_points de partida quer #################################
      starting_points <- lines
      cat ("\n")
      cat ("Wait... defining",starting_points,"starting points\n")
      cat ("-------------------------------------\n")
      
      range_min_p = 0
      range_max_p = 0
      range_min_q = 0
      range_max_q = 0
      max_freq_r = 1
      start_seq <- seq(1,starting_points)
      for (start_idx in start_seq) 
      {
        # 5% proximos a D (0,0)
        if (start_idx > particao[1]) # proximos a D (0,0)
        {
          range_min_p = 0.01
          range_max_p = 0.05
          range_min_q = 0.01
          range_max_q = 0.05
          max_freq_r = 1
        }
        if (start_idx > particao[2]*starting_points) # 5% proximos a R (0,1)
        {
          range_min_p = 0.01
          range_max_p = 0.05
          range_min_q = 0.95
          range_max_q = 0.99
          max_freq_r = 1
        }
        if (start_idx > particao[3]*starting_points) # 5% proximos a H (1,0)
        {
          range_min_p = 0.95
          range_max_p = 0.99
          range_min_q = 0.01
          range_max_q = 0.05
          max_freq_r = 1
        }
        if (start_idx > particao[4]*starting_points)  # 15% proximos a proximo a aresta DR (p baixo, q varia)
        {
          range_min_p = 0.01
          range_max_p = 0.03
          range_min_q = 0.03
          range_max_q = 0.97
          max_freq_r = 1
        }
        if (start_idx > particao[5]*starting_points) # 15% proximos a proximo a aresta DH (p varia, q baixo)
        {
          range_min_p = 0.03
          range_max_p = 0.97
          range_min_q = 0.01
          range_max_q = 0.03
          max_freq_r = 1
        }
        if (start_idx > particao[6]*starting_points) # 15% proximos a proximo a aresta HR (p alto, q alto, garantindo baixo d) 
        {
          range_min_p = 0.01
          range_max_p = 0.99
          range_min_q = 0.01
          range_max_q = 0.99
          max_freq_r = 0.03
        }
        if (start_idx > particao[7]*starting_points) # restante distribuido pelo miolo do triangulo
        {
          range_min_p = 0.03
          range_max_p = 0.97
          range_min_q = 0.03
          range_max_q = 0.97
          max_freq_r = 0.97
        }

        # inclui par de valores válido
        freq_p = 1
        freq_q = 1
        freq_r = 2
        while (freq_p+freq_q > 0.99 || freq_r > max_freq_r)
        {
          freq_p = runif(1,range_min_p,range_max_p)
          freq_q = runif(1,range_min_q,range_max_q)
          freq_r = 1-freq_p-freq_q
        } 
        value1 <- c(value1,freq_p) 
        value2 <- c(value2,freq_q)
      }
      # adiciona tres starting_points bem proximos aos vertices
      value1 <- c(value1,closemin,closemin,closemax) 
      value2 <- c(value2,closemin,closemax,closemin)
    }

    # starting_points adicionais (fixos)
    if (corner == "y")
    {  
      # vertices left (0,0), top (0,1), right (1,0)
      value1 <- c(value1,0,0,1) 
      value2 <- c(value2,0,1,0)
    }
    if (edge == "y")
    {  
      if (numcorners == 3)
      {
        # sobre a linha top-right (p=0, no doves)
        value1 <- c(value1,closemin) 
        value2 <- c(value2,closemax)
            # sobre a linha left-top
        value1 <- c(value1,closemax) 
        value2 <- c(value2,closemin)
      	# aresta top-right
        value1 <- c(value1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
        value2 <- c(value2,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 

        # sobre a linha top-left (p+q=1, no hawks)
        value1 <- c(value1,0) 
        value2 <- c(value2,closemax)
            # sobre a linha right-top
        value1 <- c(value1,0) 
        value2 <- c(value2,closemin) 
      	# aresta left-top
        value1 <- c(value1,  0,  0,  0,  0,  0,  0,  0,  0,  0)
        value2 <- c(value2,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 
      }
      # sobre a linha left-right (q=0, no mutants)
      value1 <- c(value1,closemin) 
      value2 <- c(value2,0)
      # sobre a linha right-left
      value1 <- c(value1,closemax) 
      value2 <- c(value2,0) 
      # explora valores sobre as arestas
      	# aresta left-right
      value1 <- c(value1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 
      value2 <- c(value2,  0,  0,  0,  0,  0,  0,  0,  0,  0)

    }
    # plotando
    start_seq <- seq(1,length(value1))
    for (start_idx in start_seq)
    {
      lwd <- 1
      if (temparticao == 1 && numcorners == 3)
      { 
        if (start_idx == 1 + particao[1]*starting_points) # proximos a R (0,1)
        {
          cat ("close to corner ",label_left,"\n")
        }
        if (start_idx == 1 + particao[2]*starting_points) # proximos a R (0,1)
        {
          cat ("close to corner ",label_top,"\n",sep="")
        }
        if (start_idx == 1 + particao[3]*starting_points) # proximos a H (1,0)
        {
          cat ("close to corner ",label_right,"\n")
        }
        if (start_idx == 1 + particao[4]*starting_points)  # proximos a proximo a aresta DR (p baixo, q varia)
        {
          cat ("close to edge ",label_left,"-",label_top,"\n",sep="")
        }
        if (start_idx == 1 + particao[5]*starting_points) # proximos a proximo a aresta DH (p varia, q baixo)
        {
          cat ("close to edge ",label_left,"-",label_right,"\n",sep="")
        }
        if (start_idx == 1 + particao[6]*starting_points) # proximos a proximo a aresta HR (p baixo, q baixo) 
        {
          cat ("close to edge ",label_right,"-",label_top,"\n",sep="")
        }
        if (start_idx == 1 + particao[7]*starting_points && inner == "y") # restante distribuido pelo miolo do triangulo
        {
          cat ("random (inner area)\n")
        }
      }
      freq_p = value1[start_idx]
      freq_q = value2[start_idx]
      freq_r = 1 - freq_p - freq_q
      if (freq_r < closemin) {freq_r= 0}
      if (freq_r > closemax) {freq_r= 1}
      
      # corners
      if (corner == "y")
      {
        if (freq_p==0 && freq_q==0 ) {cat ("on corner ",label_left,"\n"); lwd <- 3;}
        if (freq_p==0 && freq_q==1 ) {cat ("on corner ",label_top,"\n"); lwd <- 3;}
        if (freq_p==1 && freq_q==0 ) {cat ("on corner ",label_right,"\n"); lwd <- 3;}
      }
      # next to corners, on edges
      if (edge == "y")
      {
        if (numcorners == 3)
        {
          if (freq_p==0.000 && freq_q==closemax) {cat ("next to ",label_top," on edge ",label_top,"-",label_left,"\n"); lwd <- 3;}
          if (freq_p==0.000 && freq_q==closemin) {cat ("next to ",label_left," on edge ",label_left,"-",label_top,"\n"); lwd <- 3;}
          if (freq_p==closemin && freq_q==closemax) {cat ("next to ",label_top," on edge ",label_top,"-",label_right,"\n"); lwd <- 3;}
          if (freq_p==closemax && freq_q==closemin) {cat ("next to ",label_right," on edge ",label_right,"-",label_top,"\n"); lwd <- 3;}
        }
        if (freq_p==closemin && freq_q==0) {cat ("next to ",label_right," on edge ",label_left,"-",label_right,"\n"); lwd <- 3;}
        if (freq_p==closemax && freq_q==0) {cat ("next to ",label_left," on edge ",label_right,"-",label_left,"\n"); lwd <- 3;}
      }
#                         cat (i,": (",lwd,corner,edge,")",label_right,"=",p,"",label_top,"=",q,"",label_left,"=",r,"\n")
      
      if (paramarrow == TRUE)
      {
#                                       l <- length(ap);
#                                       cat ("length(ap)=",l,"\n");
        if (length(ap) == 0)
        {
          ap <- c(freq_p)
          aq <- c(freq_q)
        }
        else
        {
          ap <- c(ap,freq_p)
          aq <- c(aq,freq_q)
        }
#                                       cat ("ap: ",ap,"\n")
#                                       cat ("aq: ",aq,"\n")
      }

      # line
      if (lwd > 1)
      {
      ps.bary.plotsim(freq_p, freq_q, thegame=run_game, arrow=TRUE, withcol=paramcolor, kindcolor=color, kindgraph=kindgraph, dot=paramdot, lwd=lwd, w0=w0, arrowvalue=arrowvalue, ...)
      }
      else
      {
      ps.bary.plotsim(freq_p, freq_q, thegame=run_game, arrow=FALSE, withcol=paramcolor, kindcolor=color, kindgraph=kindgraph, dot=paramdot, lwd=lwd, w0=w0, arrowvalue=arrowvalue, ...)
      }
    }
    if (arrow == "y")
    {
      cat ("Drawing arrows (wait)\n")
      for (start_idx in start_seq)
      {       
        freq_p <- ap[start_idx]
        freq_q <- aq[start_idx]
        ps.bary.plotsim(freq_p, freq_q, thegame=run_game, arrow=TRUE, withcol=paramcolor, kindcolor=color, dot="n", kindgraph=kindgraph, lwd=0, w0=w0, arrowvalue=arrowvalue, ...)
        if ( (start_idx  %% 10)==0)
        {
          cat ("#")
          if ( (start_idx %% 300)==0)
            cat ("\n")
        }
      }
      cat ("\n")
    }  
    cat ("\nEnded\n")
  } # if ok
}

ps.bary.init <- function(kindline=2, col="#000000", kindgraph="ternary", numcorners=3) 
{
#                                             cat (kindgraph, numcorners,"\n")
  # initializes window, coordinates, and draws triangle, axis
  plot.new();
  if (kindgraph == "ternary")
  {
    plot.window(c(0,1), c(0,1), asp=1 );
    bary.line( c(0,0), c(1,0), l=kindline, col=col );
    if (numcorners == 3)
    {
      bary.line( c(1,0), c(0,1), l=kindline, col=col );
      bary.line( c(0,0), c(0,1), l=kindline, col=col );
    }
  }
  if (kindgraph == "tridimensional")
  {
    plot.window(c(-0.1,1.6), c(-0.1,1.6), asp=1 );
    if (numcorners == 3)
    {
      lines(c(0, 1, NA, 1, 1.5, NA, 1, 1), c(0.5, 0.5, NA, 0.5, 0, NA, 0.5, 1.5), lwd=1, col="#000000")
      lines(c(0,1,1.5,0), c(0.5,1.5,0,0.5), lwd=1, lty=2, col="#cccccc")
    }
    if (numcorners == 2)
    {
      lines(c(0, 1, 1.5), c(0.5, 0.5, 0), lwd=1, col="#000000")
    }
  }
  
}

ps.bary.makecolor <- function(scolor, paramcolor, kindcolor) 
{
  if (kindcolor == "g")
  {
    colors <- 50 
#                                         cat (scolor,"...")
    scolor <- as.integer(scolor*colors+1); # numero entre 0 e 50
#                                         cat (scolor,"...")
    if (scolor >= colors+1)
    {
      scolor = colors
    }
#     scolor = (colors+1)-scolor
    scolor = scolor*8; # numero entre 0 e 400
    scolor = scolor/500;
#                                         cat (scolor,"\n")
    
    red <- c(scolor);
    green <- c(scolor);
    blue <- c(scolor);
    
    scolor <-1
  }

  if (kindcolor == "y")
  {
    colors <- 20 
    scolor <- as.integer(scolor*colors+1);
    if (scolor >= colors+1)
    {
      scolor = colors
    }
    scolor = (colors+1)-scolor 
    
    # color-blind friendly
    red <- c(0xa3, 0xa3, 0xA5, 0x72,  
            0xF7, 0xF4, 0xEE, 0xE6,  
            0xa6, 0x90, 0x4E, 0x26, 
            0x7B, 0x61, 0x43, 0x19,
            0xCA, 0xBA, 0xAA, 0x99
           );
    green <- c(0x26, 0x0b, 0x17, 0x19,  
            0xCB, 0xA7, 0x80, 0x55,  
            0xda, 0xC9, 0xB2, 0xa1,  
            0xAF, 0x95, 0x7D, 0x65,
            0xAF, 0x95, 0x7D, 0x65
           );
    blue <- c(0x1b, 0x1b, 0x0E, 0x0E, 
            0x45, 0x36, 0x26, 0x18,  
            0x9a, 0x87, 0x65, 0x69,  
            0xDE, 0xCF, 0xBF, 0xB0,
            0xCB, 0xB4, 0x9E, 0x88
           );

    red = red/255;
    green = green/255;
    blue = blue/255;
  }
  
  rgb(red[scolor],green[scolor],blue[scolor]);
}

# show color order
ps.showcolors <- function(kindcolor=kindcolor, kindgraph=kindgraph) 
{
  temcor <- 0
  if (kindcolor == "g")
  {
    temcor <- 1
    numcolors <- 50 
    red <- c()
    green <- c()
    blue <- c()
    for (idx in seq(0, 1, by = 1/50)) 
    {
#                                         cat (i,"...")
      scolor <- as.integer(idx*numcolors+1); # numero entre 0 e 50
      if (scolor >= numcolors+1)
      {
        scolor = numcolors
      }
#       scolor = (numcolors+1)-scolor
      scolor = scolor*8; # numero entre 0 e 400
      scolor = scolor/500;
#                                         cat (scolor,"\n")
      red <- c(red,scolor);
      green <- c(green,scolor);
      blue <- c(blue,scolor);
    }
  }
  
  if (kindcolor == "y")
  {
    temcor <- 1
    numcolors <- 20
    # color-blind friendly
    red <- c(0xa3, 0xa3, 0xA5, 0x72,  
            0xF7, 0xF4, 0xEE, 0xE6,  
            0xa6, 0x90, 0x4E, 0x26, 
            0x7B, 0x61, 0x43, 0x19,
            0xCA, 0xBA, 0xAA, 0x99
           );
    green <- c(0x26, 0x0b, 0x17, 0x19,  
            0xCB, 0xA7, 0x80, 0x55,  
            0xda, 0xC9, 0xB2, 0xa1,  
            0xAF, 0x95, 0x7D, 0x65,
            0xAF, 0x95, 0x7D, 0x65
           );
    blue <- c(0x1b, 0x1b, 0x0E, 0x0E, 
            0x45, 0x36, 0x26, 0x18,  
            0x9a, 0x87, 0x65, 0x69,  
            0xDE, 0xCF, 0xBF, 0xB0,
            0xCB, 0xB4, 0x9E, 0x88
           );
    red <- red/255;
    green <- green/255;
    blue <- blue/255;
  }
  
  if (temcor == 1)
  {
    if (kindgraph == "ternary")
    {
      coord_x = 1
      coord_y = 1
      step = 0.008
    }
    if (kindgraph == "tridimensional")
    {
      coord_x = 0.1
      coord_y = 1.6
      step = 0.01
    }
    
    text (coord_x-0.1,coord_y,"faster")
    coord_y = coord_y-3*step
    for (cor in seq(numcolors, 1, by = -1)) 
    {
      idx <- cor
      if (kindcolor == "y")
      {
        idx = (numcolors+1)-idx;
      }
      rgb_color <- rgb(red[idx],green[idx],blue[idx]);
      lines(c(coord_x-0.1,coord_x), c(coord_y,coord_y), lwd=3, col=rgb_color)
      coord_y = coord_y-step
    }
    coord_y = coord_y-2*step
    text (coord_x-0.1,coord_y,"slower")
  }
}

ps.bary.line <- function( point1 , point2, arrow=FALSE, kindline=1, col="#000000", lwd=1, arrowsize=1 ) 
{
  color_arrow = "#333333"
  pt1 <- bary.toscreen(point1[1], point1[2]);
  pt2 <- bary.toscreen(point2[1], point2[2]);
  lines( c(pt1[1],pt2[1]), c(pt1[2],pt2[2]), lty=kindline, col=col, lwd=lwd );
  arrowsize = arrowsize*0.015
  if( arrow ) bary.goodarrow(pt1, pt2, length=arrowsize, col=col, border=color_arrow );
}

ps.bary.point <- function( point1, pch=21, bg="#FFFFFF", col="#000000", cex=1.0 ) 
{
  # default point is an empty circle
  pt <- bary.toscreen(point1[1], point1[2]);
  points( pt[1], pt[2], pch=pch, bg=bg, col=col, cex=cex );
}

ps.bary.plotsim <- function (sx, sy, arrow = FALSE, withcol = FALSE, kindcolor=color, kindgraph=kindgraph, dot=TRUE, lwd=1, arrowvalue=1, thegame = bary.game.hdr, w0, ...) 
{
    closemin <- 0.01
    closemax <- 0.99
    coord_x <- sx
    coord_y <- sy
    coord_r <- 1-coord_x-coord_y
    if (coord_r < closemin) {coord_r= 0}
    if (coord_r > closemax) {coord_r= 1}
    if (withcol) 
        maxv <- bary.maxvelocity(thegame = thegame, w0, ...)
    dist <- 1
    arrowcount <- 0
    first <- 1
    start_time <- Sys.time()
    msg_time <- 0
    while (dist > 1e-06) 
    {
        deltaxy <- ps.bary.sim(coord_x, coord_y, thegame = thegame, w0, ...)
        newpt <- c(deltaxy[1] + coord_x, deltaxy[2] + coord_y)
        origin <- c(coord_x, coord_y)
        coord_xo <- coord_x
        coord_yo <- coord_y
        arrowflag <- FALSE
        if (arrowcount > 0.25 && arrow) {
            arrowcount <- 0
            arrowflag <- TRUE
        }
        dist <- sqrt(deltaxy[1]^2 + deltaxy[2]^2)
        acolor <- "#000000"
        if (withcol) 
        {
          if (dist<maxv)
            acolor <- ps.bary.makecolor(dist/maxv, withcol, kindcolor) 
        }
        if (first == 1)
        {
          if (lwd > 0 && (dot=="y" || dot=="s") )
          {
#                                                         cat ("first dot=",dot,x,y,"\n")
            acolor0 <- acolor
            ponto <- c(coord_x,coord_y)
            if (kindgraph == "ternary")
            {
              ps.bary.point(ponto, col=acolor, bg="#FFFFFF", cex=0.8)
              coord_x0 <- coord_x
              coord_y0 <- coord_y
            }
            if (kindgraph == "tridimensional")
            {
              coord_z = 1 - coord_x - coord_y
              if (coord_z < closemin) {coord_z= 0}
              if (coord_z > closemax) {coord_z= 1}
              px = 1+coord_x*0.5-coord_z
              py = (0.5-coord_x*0.5)+coord_y
              points(c(px), c(py), type="p", pch=21, col=acolor, bg="#FFFFFF", cex=0.8)  
              coord_x0 <- coord_x
              coord_y0 <- coord_y
            }
          }
          first <- 0
        }
        
        if (kindgraph == "ternary")
        {
          ps.bary.line(origin, newpt, arrow = arrowflag, col = acolor, lwd=lwd, arrowsize=arrowvalue)
          arrowcount <- arrowcount + dist
        }
        if (kindgraph == "tridimensional")
        {
          lwd_3 <- lwd + coord_xo*12 - coord_yo*10
          if (lwd_3 < 2)
            lwd_3 <- 2
          transp <- as.integer(coord_xo*80 - coord_yo*20 )
          transp <- 90-transp
          zero <- ""
          if (transp < 16)
            zero <- "0"
          transp <- sprintf("%1$X", transp)
          acolor_3 <- paste(acolor,zero, transp,sep="")
          coord_zo = 1 - coord_xo - coord_yo
          if (coord_zo < closemin) {coord_zo= 0}
          if (coord_zo > closemax) {coord_zo= 1}
          pxo = 1+coord_xo*0.5-coord_zo
          pyo = (0.5-coord_xo*0.5)+coord_yo
          coord_z = 1 - coord_x - coord_y
          if (coord_z < closemin) {coord_z= 0}
          if (coord_z > closemax) {coord_z= 1}
          px = 1+coord_x*0.5-coord_z
          py = (0.5-coord_x*0.5)+coord_y
          lines(c(pxo,px), c(pyo,py), lwd=lwd_3, col=acolor_3)
        }
        coord_x <- newpt[1]
        coord_y <- newpt[2]
#                                     cat ("new (x,y):",x,y,"\n")
      # checking delay
      now_time <- Sys.time();
      dif_time <- difftime(now_time,start_time,units="secs")
      if (msg_time == 0 && dif_time > 10)
      {
        cat ("\tNow working on a demanding, slow line: please, wait...");
        msg_time <- 1 
      }
    }
    if (msg_time == 1) 
    {
      now_time <- Sys.time();
      dif_time <- as.integer(difftime(now_time,start_time,units="secs"))
      cat (" it took ",dif_time," seconds\n",sep="");
    }
    # assinala o final da linha
    if (lwd > 0 && (dot=="y" || dot=="e") )
    {
#                                                         cat ("end dot=",dot,x,y,"\n")
      ponto = c(coord_x,coord_y)
      if (kindgraph == "ternary")
      {
        ps.bary.point(ponto, col="#000000", bg="#000000", cex=1.2)
      }
      if (kindgraph == "tridimensional")
      {
        coord_z = 1 - coord_x - coord_y
        if (coord_z < closemin) {coord_z= 0}
        if (coord_z > closemax) {coord_z= 1}
        px = 1+coord_x*0.5-coord_z
        py = (0.5-coord_x*0.5)+coord_y
        points(c(px), c(py), type="p", pch=21, col="#000000", bg="#000000", cex=1.2)  
      }
    }
    
    # assinala onde a linha comecou
    if (lwd > 0 && (dot=="y" || dot=="s") )
    {
#                                                         cat ("first dot=",dot,x,y,"\n")
      ponto <- c(coord_x0,coord_y0)
      if (kindgraph == "ternary")
      {
        ps.bary.point(ponto, col=acolor0, bg="#FFFFFF", cex=0.8)
      }
      if (kindgraph == "tridimensional")
      {
        coord_z = 1 - coord_x0 - coord_y0
        if (coord_z < closemin) {coord_z= 0}
        if (coord_z > closemax) {coord_z= 1}
        px = 1+coord_x0*0.5-coord_z
        py = (0.5-coord_x0*0.5)+coord_y0
        points(c(px), c(py), type="p", pch=21, col=acolor0, bg="#FFFFFF", cex=0.8)  
      }
    }

  c(c(sx, sy), c(coord_x, coord_y))
}

ps.bary.maxvelocity <- function (thegame = bary.game.hdr, w0, ...) 
{
#     cat ("ps.bary.maxvelocity", v, c, w0,"\n")
    maxdist <- 0
    for (idx in seq(0, 1, by = 0.025)) {
        for (jdx in seq(0, 1, by = 0.025)) {
            freq_p <- idx
            freq_q <- jdx
            if (freq_p + freq_q > 1) 
                next
            deltaxy <- ps.bary.sim(freq_p, freq_q, thegame = thegame, w0, ...)
#                                       cat ("deltaxy 1:",deltaxy[1],"2:",deltaxy[2],"3:",deltaxy[3],"\n")
            dist <- sqrt(deltaxy[1]^2 + deltaxy[2]^2)
#                                       cat ("dist:",dist,"maxdist:",maxdist,"\n")
            if (dist > maxdist) 
                maxdist <- dist
        }
    }
    maxdist
}

ps.bary.sim <- function (startx, starty, thegame = bary.game.hdr, w0, ...) 
{
  
#     cat ("ps.bary.sim", v, c, w0,"\n")
    closemin <- 0.01
    closemax <- 0.99
    freq_p <- startx
    freq_q <- starty
    freq_r <- 1-freq_p-freq_q
    if (freq_r < closemin) {freq_r= 0}
    if (freq_r > closemax) {freq_r= 1}

    w_game <- thegame(freq_p, freq_q, freq_r, w0, ...)
    wbar <- freq_p * w_game[1] + freq_q * w_game[2] + freq_r * w_game[3]
    dp <- freq_p * w_game[1]/wbar - freq_p
    dq <- freq_q * w_game[2]/wbar - freq_q
    dr <- freq_r * w_game[3]/wbar - freq_r

    c(dp, dq, dr)
}

