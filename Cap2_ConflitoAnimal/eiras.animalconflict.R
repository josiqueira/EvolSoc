# Animal Conflict Game

# clean all memory variables
rm(list = ls())

stopif <- 1e-3
draw.each <- 400

# disable warnings
oldw <- getOption("warn")
options(warn = -1)

library(readxl)
library(openxlsx)
source("eiras.friendlycolor.R")
source("eiras.exit.R")
source("eiras.tern.graph.R")
source("eiras.ternary.replot.R")

cat ("\n* Welcome to Eira's animal conflict game *\n\n")
cat ("Please, provide the name of a configuration file\n")
cat ("or press ESC to abort.\n")
fileconfig <- readline(prompt="Filename (without the extension ,xls or ,xlsx): ")

# check if there is xls or xlsx with fileconfig
ok <- 0
fileread <- fileconfig
if (file.exists(fileread))
{
  ok <- 1
}
if (ok == 0) # try .xls
{
  fileread <- paste(fileconfig,".xls",sep="")
  if (file.exists(fileread))
  {
    ok <- 1
  }
}
if (ok == 0) # try .xlsx
{
  fileread <- paste(fileconfig,".xlsx",sep="")
  if (file.exists(fileread))
  {
    ok <- 1
  }
}
if (ok == 1)
{
  df_config <- readxl::read_excel(fileread)
} else
{
  cat("\n\nI am sorry, but ",fileconfig," was not find here.\n",sep="")
  eiras.exit()
}

# read confirmed
cat("\n------------------------------------------\n",sep="")
cat( "Configuration file: ",fileread," found",sep="")
cat("\n------------------------------------------\n",sep="")



# Getting data ----------------------------------
# observation
obs <- as.character(df_config[which(
  df_config[,1]=="Observation"
  ),])
cat(obs[1],":\n\t",obs[2],"\n",sep="")

# player names
player.name <- as.character(df_config[which(
  df_config[,1]=="Player names"
),2:4])
player.name[player.name=="NA"] <- NA
player.name <- player.name[!is.na(player.name)]
numvertices <- sum(!is.na(player.name))
cat("Players:\n")
for (p in 1:numvertices)
{
  cat("\t",player.name[p],"\n",sep="")
}

# check if there is exploration
vertices <- edges <- corners <- near.edges <- center <- rep(0,3)
vertices <- as.character(df_config[which(
  df_config[,1]=="vertices"),1:3])
edges <- as.character(df_config[which(
  df_config[,1]=="edges"),1:3])
corners <- as.character(df_config[which(
  df_config[,1]=="corners"),1:3])
if (numvertices==3)
{
  near.edges <- as.character(df_config[which(
    df_config[,1]=="near edges"),1:3])
  center <- as.character(df_config[which(
    df_config[,1]=="center"),1:3])
}
if (sum(as.numeric(c(
        vertices[2],
        corners[2],
        edges[2],
        near.edges[2],
        center[2]))) > 0)
{
  env.explore <- "y"
  cat("Exploration activated:\n",sep="")
  cat(" - ",vertices[3],": ",vertices[2],"\n",sep=""); 
  vertices <- as.numeric(vertices[2]);
  cat(" - ",corners[3],": ",corners[2],"\n",sep=""); 
  corners <- as.numeric(corners[2]);
  cat(" - ",edges[3],": ",edges[2],"\n",sep=""); 
  edges <- as.numeric(edges[2]);
  if (numvertices==3)
  {
    cat(" - ",near.edges[3],": ",near.edges[2],"\n",sep=""); 
    cat(" - ",center[3],": ",center[2],"\n",sep=""); 
  }
  near.edges <- as.numeric(near.edges[2]);
  center <- as.numeric(center[2]);
  cat("\t(initial proportions ignored)\n",sep="")
  # ignore initial proportions
  player.p <- matrix(data=0,
                     ncol=numvertices, nrow=1, byrow=TRUE)
  colnames(player.p) <- player.name
  rownames(player.p) <- NULL
} else
{
  env.explore <- "n"
  vertices <- corners <- edges <- near.edges <- center <- 0

  ini.prop <- as.character(df_config[which(
    df_config[,1]=="Initial Proportion"
  ),])
  # player's initial frequency
  player.p <- matrix(data=round(as.numeric(ini.prop[2:(numvertices+1)]),8),
                     ncol=numvertices, nrow=1, byrow=TRUE)
  colnames(player.p) <- player.name
  rownames(player.p) <- NULL
  # correct to sum==1
  player.p <- round(player.p/sum(player.p),8)
  
  cat("Initial proportions:\n",sep="")
  for (c in 1:3)
  {
    cat(" - ",colnames(player.p)[c],": ",player.p[c],"\n",sep=""); 
  }
}

# w0, v, c
cat("Parameters:\n",sep="")
txt <- as.character(df_config[which(
  df_config[,1]=="w0"),1:3])
w0 <- as.numeric(txt[2])
cat(" - ",txt[3]," (",txt[1],")",": ",w0,"\n",sep="");

txt <- as.character(df_config[which(
  df_config[,1]=="v"),1:3])
v <- as.numeric(txt[2])
cat(" - ",txt[3]," (",txt[1],")",": ",v,"\n",sep="");

txt <- as.character(df_config[which(
  df_config[,1]=="c"),1:3])
c <- as.numeric(txt[2])
cat(" - ",txt[3]," (",txt[1],")",": ",c,"\n",sep="");

# jogo continuo
# txt <- as.character(df_config[which(
#   df_config[,1]=="h"),1:3])
# h <- as.numeric(txt[2])

# asymmetry
txt <- as.character(df_config[which(
  df_config[,1]=="x"),1:3])
x <- as.numeric(txt[2])
if(is.na(x)) {x <- 0}
cat(" - ",txt[3]," (",txt[1],")",": ",x,"\n",sep="");
kind <- c(NA,NA)
if (x>0)
{
  txt <- as.character(df_config[which(
    df_config[,1]=="kind[1]"),1:3])
  kind[1]  <- as.character(txt[2])
  cat(" - ",txt[3]," (",txt[1],")",": ",kind[1],"\n",sep="");
  txt <- as.character(df_config[which(
    df_config[,1]=="kind[2]"),1:3])
  kind[2]  <- as.character(txt[2])
  cat(" - ",txt[3]," (",txt[1],")",": ",kind[2],"\n",sep="");
} else
{
  cat(" - no asymmetries\n", sep="");
}

txt <- as.character(df_config[which(
  df_config[,1]=="Color"),1:3])
env.color <- as.character(txt[2])
if (env.color == "y") {cat("- showing colored graphs\n",sep="")}
if (env.color == "n") {cat("- showing grayed graphs\n",sep="")}

txt <- as.character(df_config[which(
  df_config[,1]=="Background"),1:3])
env.bg <- as.character(txt[2])
if (env.bg == "y") {cat("- showing background\n",sep="")}
if (env.bg == "n") {cat("- showing background\n",sep="")}

cat("Game matrices:\n",sep="")
dim <- numvertices
if (x > 0) {dim <- dim+1}

# assuming that the structure of all matrices are equal

# gain.matrix
pos <- which(df_config[,1]=="Gain Matrix")
txt <- as.character(df_config[pos,1])
gain.matrix <- matrix(data=0, ncol=dim, nrow=dim)
colnames(gain.matrix) <- as.vector(unlist(df_config[(pos+1):(pos+dim),1]))
rownames(gain.matrix) <- colnames(gain.matrix)
for(r in 1:dim)
{
  gain.matrix[r,] <- as.vector(unlist(df_config[pos+r,2:(dim+1)]))
}
# gain.matrix[is.na(gain.matrix)] <- "-"
cat("\n",txt,"\n",sep="")
prmatrix(gain.matrix, quote=FALSE)
# preserve textual form
gain.matrix.txt <- gain.matrix
# convert variable names to number content
txt.content <- as.vector(gain.matrix[1:ncol(gain.matrix),])
num.content <- c()
for(i in 1:length(txt.content))
{
  # test if string can be converted to numeric
  # https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",txt.content[i]))
  {
    # get as number
    num.content <- c(num.content, as.numeric(txt.content[i]))
  } else
  {
    # get as variable name
    if(!is.na(txt.content[i]))
    {
      num.content <- c(num.content, get(txt.content[i]))
    } else
    {
      num.content <- c(num.content, NA)
    }
  }
}
# rebuild and show matrix
mnames <- colnames(gain.matrix)
gain.matrix <- matrix(data=num.content, ncol=dim, nrow=dim, byrow = FALSE )
colnames(gain.matrix) <- mnames
rownames(gain.matrix) <- mnames
cat("\t--- converted to:\n",sep="")
prmatrix(gain.matrix, quote=FALSE)

# loss matrix
pos <- which(df_config[,1]=="Loss Matrix")
txt <- as.character(df_config[pos,1])
loss.matrix <- matrix(data=0, ncol=dim, nrow=dim)
colnames(loss.matrix) <- as.vector(unlist(df_config[(pos+1):(pos+dim),1]))
rownames(loss.matrix) <- colnames(loss.matrix)
for(r in 1:dim)
{
  loss.matrix[r,] <- as.vector(unlist(df_config[pos+r,2:(dim+1)]))
}
cat("\n",txt,"\n",sep="")
prmatrix(loss.matrix, quote=FALSE)
# preserve textual form
loss.matrix.txt <- loss.matrix
# convert variable names to number content
txt.content <- as.vector(loss.matrix[1:ncol(loss.matrix),])
num.content <- c()
for(i in 1:length(txt.content))
{
  # test if string can be converted to numeric
  # https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",txt.content[i]))
  {
    # get as number
    num.content <- c(num.content, as.numeric(txt.content[i]))
  } else
  {
    # get as variable name
    if(!is.na(txt.content[i]))
    {
      num.content <- c(num.content, get(txt.content[i]))
    } else
    {
      num.content <- c(num.content, NA)
    }
  }
}
# rebuild and show matrix
mnames <- colnames(loss.matrix)
loss.matrix <- matrix(data=num.content, ncol=dim, nrow=dim, byrow = FALSE )
colnames(loss.matrix) <- mnames
rownames(loss.matrix) <- mnames
cat("\t--- converted to:\n",sep="")
prmatrix(loss.matrix, quote=FALSE)

pos <- which(df_config[,1]=="Prob. Win")
txt <- as.character(df_config[pos,1])
pwin.matrix <- matrix(data=0, ncol=dim, nrow=dim)
colnames(pwin.matrix) <- as.vector(unlist(df_config[(pos+1):(pos+dim),1]))
rownames(pwin.matrix) <- colnames(pwin.matrix)
for(r in 1:dim)
{
  pwin.matrix[r,] <- as.vector(unlist(df_config[pos+r,2:(dim+1)]))
}
# preventing numbers as 0.40000000000000002
for (mr in 1:nrow(pwin.matrix))
{
  for (mc in 1:ncol(pwin.matrix))
  {
    if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",pwin.matrix[mr,mc]))
    {
      tmp <- as.numeric(pwin.matrix[mr,mc])
      pwin.matrix[mr,mc] <- paste(tmp)
    }
  }
}
cat("\n",txt,"\n",sep="")
prmatrix(pwin.matrix, quote=FALSE)
# convert variable names to number content
txt.content <- as.vector(pwin.matrix[1:ncol(pwin.matrix),])
num.content <- c()
for(i in 1:length(txt.content))
{
  # test if string can be converted to numeric
  # https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
  if (grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",txt.content[i]))
  {
    # get as number
    num.content <- c(num.content, as.numeric(txt.content[i]))
  } else
  {
    # get as variable name
    if(!is.na(txt.content[i]))
    {
      num.content <- c(num.content, get(txt.content[i]))
    } else
    {
      num.content <- c(num.content, NA)
    }
  }
}
# rebuild and show matrix
mnames <- colnames(pwin.matrix)
pwin.matrix <- matrix(data=num.content, ncol=dim, nrow=dim, byrow = FALSE )
colnames(pwin.matrix) <- mnames
rownames(pwin.matrix) <- mnames
cat("\t--- converted to:\n",sep="")
prmatrix(pwin.matrix, quote=FALSE)

# liquid gain*pwin + gain*(1-pwin)
# buiding liquid matrix in text format
liquid.matrix.txt <- c()
liquid.matrix <- c()
pwin <- as.numeric(pwin.matrix)
dimadd <- 0
if (dim==4) {dimadd <- 1}
pos <- 1
for (mc in 1:(ncol(pwin.matrix)+dimadd))
{
  for (mr in 1:(nrow(pwin.matrix)+dimadd))
  {
    if (dim<=3)
    {
      liquid.matrix.txt <- c(liquid.matrix.txt, 
                             paste(pwin.matrix[mr,mc],"*",gain.matrix.txt[mr,mc],
                                   "-",
                                   1-pwin.matrix[mr,mc],"*",loss.matrix.txt[mr,mc],
                                   sep=""))
      liquid.matrix <- c(liquid.matrix, 
                         pwin.matrix[mr,mc]*gain.matrix[mr,mc] -
                           (1-pwin.matrix[mr,mc])*loss.matrix[mr,mc])
    } else # dim == 4
    {
      if (mc<=4 & mr<=4)
      {
        liquid.matrix.txt <- c(liquid.matrix.txt, 
                               paste(pwin.matrix[mr,mc],"*",gain.matrix.txt[mr,mc],
                                     "-",
                                     1-pwin.matrix[mr,mc],"*",loss.matrix.txt[mr,mc],
                                     sep=""))
        liquid.matrix <- c(liquid.matrix, 
                           pwin.matrix[mr,mc]*gain.matrix[mr,mc] -
                             (1-pwin.matrix[mr,mc])*loss.matrix[mr,mc])
      } else
      {
        liquid.matrix.txt <- c(liquid.matrix.txt,"")
        liquid.matrix <- c(liquid.matrix,0)
      }
    }
    pos <- pos+1      
  } # mr
} # mc
# NAs
found.nas <- as.numeric(regexpr('NA', liquid.matrix.txt))
liquid.matrix.txt[which(found.nas>0)] <- "-"
liquid.matrix[which(found.nas>0)] <- 0
mnames <- colnames(gain.matrix)
dimadd <- 0
if (dim==4) 
{
  dimadd <- 1
  mnames <- c(mnames,player.name[3])
}
liquid.matrix.txt <- matrix(data=liquid.matrix.txt, 
                            ncol=dim+dimadd, nrow=dim+dimadd, byrow = FALSE )
colnames(liquid.matrix.txt) <- mnames
rownames(liquid.matrix.txt) <- mnames
liquid.matrix <- matrix(data=liquid.matrix, 
                        ncol=dim+dimadd, nrow=dim+dimadd, byrow = FALSE )
colnames(liquid.matrix) <- mnames
rownames(liquid.matrix) <- mnames
if(dim==4)
{
  # times prob.[assessor is bigger]=(0.5)
  for (mc in 1:4)
  {
    for (mr in 1:4)
    {
      if (mr<=2&mc<=2) {next}
      if (liquid.matrix.txt[mr,mc] != "-")
      {
        liquid.matrix.txt[mr,mc] <- paste("(",liquid.matrix.txt[mr,mc],")*0.5",sep="") 
        liquid.matrix[mr,mc] <- liquid.matrix[mr,mc]*0.5 
      }
    }
  }
  # collapsing row 5
  for (mc in 1:4)
  {
    addsignal <- "+"
    tmp1 <- liquid.matrix.txt[3,mc]
    if (tmp1=="-"){addsignal <- tmp1 <- ""}
    tmp2 <- liquid.matrix.txt[4,mc]
    if (tmp2=="-"){addsignal <- tmp2 <- ""}
    liquid.matrix.txt[5,mc] <- paste(tmp1,addsignal,tmp2,sep="") 
    liquid.matrix[5,mc] <- liquid.matrix[3,mc]+liquid.matrix[4,mc] 
  }
  # collapsing col 5
  for (mr in 1:4)
  {
    addsignal <- "+"
    tmp1 <- liquid.matrix.txt[mr,3]
    if (tmp1=="-"){addsignal <- tmp1 <- ""}
    tmp2 <- liquid.matrix.txt[mr,4]
    if (tmp2=="-"){addsignal <- tmp2 <- ""}
    liquid.matrix.txt[mr,5] <- paste(tmp1,addsignal,tmp2,sep="") 
    liquid.matrix[mr,5] <- liquid.matrix[mr,3]+liquid.matrix[mr,4] 
  }
  # position [5,5]
  liquid.matrix.txt[5,5] <- paste(liquid.matrix.txt[5,3],"+",liquid.matrix.txt[5,4],sep="") 
  liquid.matrix[5,5] <- liquid.matrix[5,3]+liquid.matrix[5,4] 
  # removing partial rows & cols
  liquid.matrix.txt <- liquid.matrix.txt[,c(1,2,5)]
  liquid.matrix.txt <- liquid.matrix.txt[c(1,2,5),]
  liquid.matrix <- liquid.matrix[,c(1,2,5)]
  liquid.matrix <- liquid.matrix[c(1,2,5),]
}
cat("\nResulting game matrix:\n",sep="")
prmatrix(liquid.matrix.txt, quote=FALSE)
cat("\t--- converted to:\n",sep="")
prmatrix(round(liquid.matrix,8), quote=FALSE)

# Starting simulation ---------------------------------

# open graph
grphtitle <- paste("w0=",w0,", v=",v,", c=",c,sep="")
if (x>0)
{
  grphtitle <- paste(grphtitle,", x=",x,", p.win[",kind[1],"]=",pwin.matrix[3,1],sep="")
}
g.bg = "#d8d8d8"
if (env.bg=="n")
{
  g.bg = "#ffffff"
}
eiras.tern.graph(
  main=grphtitle,
  label.left = player.name[1],
  label.right = player.name[2],
  label.top = player.name[3],
  g.bg=g.bg
)
# speed colors
slow.to.fast.color <- c(7,8,15,21,30)
slow.to.fast.gray <- c(32,34,36,37,38)
if (env.color=="y")
{
  slow.to.fast <- slow.to.fast.color
} else
{
  slow.to.fast <- slow.to.fast.gray
}

# single game
validgames <- rep(1,13)
if (env.explore == "n") 
{
  numgames <- 1
  repgames <- 1
  repstep <- 0
} 
# exploration
if (env.explore == "y") 
{
  numgames <- 13
  repgames <- 0 # to be set
  repstep <- 0
  if (numvertices==2)
  {
    validgames[c(3,6,8:13)] <- 0
  }
} 

for (games in 1:numgames)
{
  if (env.explore == "y") 
  {
    # - vertices(3), 1:3
    if (games >= 1 & games <= 3)
    {
      repgames <- vertices
      if (repgames>1) {repgames <- 1}
    }
    
    # - corners(3), 4:6
    if (games >= 4 & games <= 6)
    {
      repgames <- corners
      if (repgames!=0 & repgames<2) {repgames <- 2}
    }
    # - edges(3), 7:9
    if (games >= 7 & games <= 9)
    {
      repgames <- edges
    }
    # - near.edges(3), 10:12 
    if (games >= 10 & games <= 12)
    {
      repgames <- near.edges
    }
    # - center(1), 13
    if (games == 13)
    {
      repgames <- center
    }
    if (repgames>0)
    {
      repstep <- 1/repgames
    }
  } # env.explore == "y"  
  
  # skip if any given exploration is absent
  if (repgames==0) {next}

  for (repetition in 1:repgames)
  {
    if (env.explore=="y")
    {
      # - vertices(3), 1:3
      # - corners(3), 4:6
      # - edges(3), 7:9
      # - near.edges(3), 10:12 
      # - center(1), 13
      # lv        rv        tv
      # A:(0,0,1) B:(0,1,0) C:(1,0,0)

      # - vertices(3), 1:3
      if (games >= 1 & games <= 3)
      {
        if (validgames[games]==1 & games==1) # left corner
        {
          if (repetition==1)
          {
            cat ("\non left corner, ",repgames," repetitions: ", sep="")
          }
          player.p[1] <- 1
          player.p[2] <- 0
          if (numvertices==3)
          {
            player.p[3] <- 0
          }
        }
        if (validgames[games]==1 & games==2) # right corner
        {
          if (repetition==1)
          {
            cat ("\non right corner, ",repgames," repetitions: ", sep="")
          }
          player.p[1] <- 0
          player.p[2] <- 1
          if (numvertices==3)
          {
            player.p[3] <- 0
          }
        }
        if (validgames[games]==1 & games==3) # top corner
        {
          if (repetition==1)
          {
            cat ("\non top corner, ",repgames," repetitions: ", sep="")
          }
          player.p[1] <- 0
          player.p[2] <- 0
          player.p[3] <- 1
        }
      }
      # - corners(3), 4:6
      if (games >= 4 & games <= 6)
      {
        if (validgames[games]==1 & games==4) # left-right axis
        {
          if (repetition==1)
          {
            cat ("\nclose to left corner, ",repgames," repetitions: ", sep="")
          }
          # high left, low (top and right)
          player.p[1] <- runif(1,0.95,0.99)
          if (numvertices==2)
          {
            player.p[2] <- 1-player.p[1]
          }
          if (numvertices==3)
          {
            player.p[2] <- runif(1,0.03,0.05)
            player.p[3] <- runif(1,0.03,0.05)
          }
          if (repetition==1)
          {
            if (numvertices==2)
            {
              player.p[2] <- 0.025
              player.p[1] <- 1-player.p[2]
            }  
            if (numvertices==3)
            {
              player.p[2] <- 0
              player.p[3] <- 0.025
              player.p[1] <- 1-(player.p[3]+player.p[2])
            }            
          }
          if (numvertices==3 & repetition==2)
          {
            player.p[3] <- 0
            player.p[2] <- 0.025
            player.p[1] <- 1-(player.p[3]+player.p[2])
          }
          if (numvertices==3 & repetition==3)
          {
            player.p[2] <- runif(1,0.01,0.02)
          }
          if (numvertices==3 & repetition==4)
          {
            player.p[3] <- runif(1,0.01,0.02)
          }
        }
        if (validgames[games]==1 & games==5) # close to right corner
        {
          if (repetition==1)
          {
            cat ("\nclose to right corner, ",repgames," repetitions: ", sep="")
          }
          # high right, low (top and left)
          player.p[2] <- runif(1,0.95,0.99)
          if (numvertices==2)
          {
            player.p[1] <- 1-player.p[2]
          }
          if (numvertices==3)
          {
            player.p[1] <- runif(1,0.03,0.05)
            player.p[3] <- runif(1,0.03,0.05)
          }
          if (repetition==1)
          {
            if (numvertices==2)
            {
              player.p[1] <- 0.025
              player.p[2] <- 1-player.p[1]
            }  
            if (numvertices==3)
            {
              player.p[3] <- 0
              player.p[1] <- 0.025
              player.p[2] <- 1-(player.p[3]+player.p[1])
            }            
          }
          if (numvertices==3 & repetition==2)
          {
            player.p[1] <- 0
            player.p[3] <- 0.025
            player.p[2] <- 1-(player.p[3]+player.p[1])
          }
          if (numvertices==3 & repetition==3)
          {
            player.p[3] <- runif(1,0.01,0.02)
          }
          if (numvertices==3 & repetition==4)
          {
            player.p[1] <- runif(1,0.01,0.02)
          }
        }  
        if (validgames[games]==1 & games==6) # right-top axis
        {
          if (repetition==1)
          {
            cat ("\nclose to top corner, ",repgames," repetitions: ", sep="")
          }
          # high top, low (left and right)
          player.p[1] <- runif(1,0.03,0.05)
          player.p[2] <- runif(1,0.03,0.05)
          player.p[3] <- runif(1,0.95,0.99)
          if (repetition==1)
          {
            player.p[1] <- 0
            player.p[2] <- 0.025
            player.p[3] <- 1-(player.p[2]+player.p[1])
          }
          if (repetition==2)
          {
            player.p[2] <- 0
            player.p[1] <- 0.025
            player.p[3] <- 1-(player.p[2]+player.p[1])
          }
          if (repetition==3)
          {
            player.p[1] <- runif(1,0.01,0.02)
          }
          if (repetition==4)
          {
            player.p[2] <- runif(1,0.01,0.02)
          }
        }  
      }
      
      # - edges(3), 7:9
      if (games >= 7 & games <= 9)
      {
        if (validgames[games]==1 & games==7) # left-right axis
        {
          if (repetition==1)
          {
            cat ("\non left-right axis, ",repgames," repetitions: ", sep="")
          }
          # top = 0
          if (repetition<=5) # regular
          {
            if (numvertices==2)
            {
              player.p[1] <- (1/(5+1))*repetition
              player.p[2] <- 1-player.p[1]
            }
            if (numvertices==3)
            {
              player.p[3] <- 0
              player.p[1] <- (1/(5+1))*repetition
              player.p[2] <- 1-(player.p[1]+player.p[3])
            }
          } else
          {
            if (numvertices==2)
            {
              player.p[1] <- runif(1,0.05,0.95)
              player.p[2] <- 1-player.p[1]
            }
            if (numvertices==3)
            {
              player.p[3] <- 0
              player.p[1] <- runif(1,0.05,0.95)
              player.p[2] <- runif(1,0.05,0.95)
              denominator <- sum(player.p[1],player.p[2])
              player.p[1] <- player.p[1]/denominator
              player.p[2] <- player.p[2]/denominator
            }
          }
        }
        if (validgames[games]==1 & games==8) # left-top axis
        {
          if (repetition==1)
          {
            cat ("\non left-top axis, ",repgames," repetitions: ", sep="")
          }
          # right = 0
          if (repetition<=5) # regular
          {
            player.p[2] <- 0
            player.p[1] <- (1/(5+1))*repetition
            player.p[3] <- 1-(player.p[1]+player.p[2])
          } else
          {
            player.p[2] <- 0
            player.p[1] <- runif(1,0.05,0.95)
            player.p[3] <- runif(1,0.05,0.95)
            denominator <- sum(player.p[1],player.p[3])
            player.p[1] <- player.p[1]/denominator
            player.p[3] <- player.p[3]/denominator
          }
        }  
        if (validgames[games]==1 & games==9) # right-top axis
        {
          if (repetition==1)
          {
            cat ("\non right-top axis, ",repgames," repetitions: ", sep="")
          }
          # left = 0
          if (repetition<=5) # regular
          {
            player.p[1] <- 0
            player.p[2] <- (1/(5+1))*repetition
            player.p[3] <- 1-(player.p[1]+player.p[2])
          } else
          {
            player.p[1] <- 0
            player.p[2] <- runif(1,0.05,0.95)
            player.p[3] <- runif(1,0.05,0.95)
            denominator <- sum(player.p[2],player.p[3])
            player.p[2] <- player.p[2]/denominator
            player.p[3] <- player.p[3]/denominator
          }
        }  
      }
      
      # - near.edges(3), 
      if (games >= 10 & games <= 12)
      {
        if (validgames[games]==1 & games==10) # left-right axis
        {
          if (repetition==1)
          {
            cat ("\nclose to left-right axis, ",repgames," repetitions: ", sep="")
          }
          # low top, rnd (left and right)
          if (repetition<=10) # regular
          {
            player.p[3] <- 0.025
            player.p[1] <- (1/(10+1))*repetition
            player.p[2] <- 1-(player.p[1]+player.p[3])
          } else
          {
            player.p[3] <- runif(1,0.001,0.05)
            player.p[1] <- runif(1,0.1,0.99)
            player.p[2] <- runif(1,0.1,0.99)
            denominator <- sum(player.p[1],player.p[2])
            player.p[1] <- player.p[1]/denominator - player.p[3]/2
            player.p[2] <- player.p[2]/denominator - player.p[3]/2
          }
        }
        if (validgames[games]==1 & games==11) # left-top axis
        {
          if (repetition==1)
          {
            cat ("\nclose to left-top axis, ",repgames," repetitions: ", sep="")
          }
          # low right, rnd (left and top)
          if (repetition<=10) # regular
          {
            player.p[2] <- 0.025
            player.p[1] <- (1/(10+1))*repetition
            player.p[3] <- 1-(player.p[1]+player.p[2])
          } else
          {
            player.p[2] <- runif(1,0.001,0.05)
            player.p[1] <- runif(1,0.1,0.99)
            player.p[3] <- runif(1,0.1,0.99)
            denominator <- sum(player.p[1],player.p[3])
            player.p[1] <- player.p[1]/denominator - player.p[2]/2
            player.p[3] <- player.p[3]/denominator - player.p[2]/2
          }
        }  
        if (validgames[games]==1 & games==12) # right-top axis
        {
          if (repetition==1)
          {
            cat ("\nclose to right-top axis, ",repgames," repetitions: ", sep="")
          }
          # low left, rnd (right and top)
          if (repetition<=10) # regular
          {
            player.p[1] <- 0.025
            player.p[2] <- (1/(10+1))*repetition
            player.p[3] <- 1-(player.p[1]+player.p[2])
          } else
          {
            player.p[1] <- runif(1,0.001,0.05)
            player.p[2] <- runif(1,0.1,0.99)
            player.p[3] <- runif(1,0.1,0.99)
            denominator <- sum(player.p[2],player.p[3])
            player.p[2] <- player.p[2]/denominator - player.p[1]/2
            player.p[3] <- player.p[3]/denominator - player.p[1]/2
          }
        }  
      }
      # - center(1), 
      if (validgames[games]==1 & games == 13)
      {
        if (repetition==1)
        {
          cat ("\nenvironment center, ",repgames," repetitions: ", sep="")
        }
        if (repetition<=36) # regular
        {
          if(repetition==1){	player.p[1] <- 0.1;	player.p[2] <- 0.1;	player.p[3] <- 0.8;	}
          if(repetition==2){	player.p[1] <- 0.1;	player.p[2] <- 0.2;	player.p[3] <- 0.7;	}
          if(repetition==3){	player.p[1] <- 0.2;	player.p[2] <- 0.1;	player.p[3] <- 0.7;	}
          if(repetition==4){	player.p[1] <- 0.1;	player.p[2] <- 0.3;	player.p[3] <- 0.6;	}
          if(repetition==5){	player.p[1] <- 0.2;	player.p[2] <- 0.2;	player.p[3] <- 0.6;	}
          if(repetition==6){	player.p[1] <- 0.3;	player.p[2] <- 0.1;	player.p[3] <- 0.6;	}
          if(repetition==7){	player.p[1] <- 0.1;	player.p[2] <- 0.4;	player.p[3] <- 0.5;	}
          if(repetition==8){	player.p[1] <- 0.2;	player.p[2] <- 0.3;	player.p[3] <- 0.5;	}
          if(repetition==9){	player.p[1] <- 0.3;	player.p[2] <- 0.2;	player.p[3] <- 0.5;	}
          if(repetition==10){	player.p[1] <- 0.4;	player.p[2] <- 0.1;	player.p[3] <- 0.5;	}
          if(repetition==11){	player.p[1] <- 0.1;	player.p[2] <- 0.5;	player.p[3] <- 0.4;	}
          if(repetition==12){	player.p[1] <- 0.2;	player.p[2] <- 0.4;	player.p[3] <- 0.4;	}
          if(repetition==13){	player.p[1] <- 0.3;	player.p[2] <- 0.3;	player.p[3] <- 0.4;	}
          if(repetition==14){	player.p[1] <- 0.4;	player.p[2] <- 0.2;	player.p[3] <- 0.4;	}
          if(repetition==15){	player.p[1] <- 0.5;	player.p[2] <- 0.1;	player.p[3] <- 0.4;	}
          if(repetition==16){	player.p[1] <- 0.1;	player.p[2] <- 0.6;	player.p[3] <- 0.3;	}
          if(repetition==17){	player.p[1] <- 0.2;	player.p[2] <- 0.5;	player.p[3] <- 0.3;	}
          if(repetition==18){	player.p[1] <- 0.3;	player.p[2] <- 0.4;	player.p[3] <- 0.3;	}
          if(repetition==19){	player.p[1] <- 0.4;	player.p[2] <- 0.3;	player.p[3] <- 0.3;	}
          if(repetition==20){	player.p[1] <- 0.5;	player.p[2] <- 0.2;	player.p[3] <- 0.3;	}
          if(repetition==21){	player.p[1] <- 0.6;	player.p[2] <- 0.1;	player.p[3] <- 0.3;	}
          if(repetition==22){	player.p[1] <- 0.1;	player.p[2] <- 0.7;	player.p[3] <- 0.2;	}
          if(repetition==23){	player.p[1] <- 0.2;	player.p[2] <- 0.6;	player.p[3] <- 0.2;	}
          if(repetition==24){	player.p[1] <- 0.3;	player.p[2] <- 0.5;	player.p[3] <- 0.2;	}
          if(repetition==25){	player.p[1] <- 0.4;	player.p[2] <- 0.4;	player.p[3] <- 0.2;	}
          if(repetition==26){	player.p[1] <- 0.5;	player.p[2] <- 0.3;	player.p[3] <- 0.2;	}
          if(repetition==27){	player.p[1] <- 0.6;	player.p[2] <- 0.2;	player.p[3] <- 0.2;	}
          if(repetition==28){	player.p[1] <- 0.7;	player.p[2] <- 0.1;	player.p[3] <- 0.2;	}
          if(repetition==29){	player.p[1] <- 0.1;	player.p[2] <- 0.8;	player.p[3] <- 0.1;	}
          if(repetition==30){	player.p[1] <- 0.2;	player.p[2] <- 0.7;	player.p[3] <- 0.1;	}
          if(repetition==31){	player.p[1] <- 0.3;	player.p[2] <- 0.6;	player.p[3] <- 0.1;	}
          if(repetition==32){	player.p[1] <- 0.4;	player.p[2] <- 0.5;	player.p[3] <- 0.1;	}
          if(repetition==33){	player.p[1] <- 0.5;	player.p[2] <- 0.4;	player.p[3] <- 0.1;	}
          if(repetition==34){	player.p[1] <- 0.6;	player.p[2] <- 0.3;	player.p[3] <- 0.1;	}
          if(repetition==35){	player.p[1] <- 0.7;	player.p[2] <- 0.2;	player.p[3] <- 0.1;	}
          if(repetition==36){	player.p[1] <- 0.8;	player.p[2] <- 0.1;	player.p[3] <- 0.1;	}        } else
        {
          # all random
          player.p[1] <- runif(1,0.1,0.9)
          player.p[2] <- runif(1,0.1,0.9)
          player.p[3] <- runif(1,0.1,0.9)
          denominator <- sum(player.p[1],player.p[2],player.p[3])
          player.p[1] <- player.p[1]/denominator
          player.p[2] <- player.p[2]/denominator
          player.p[3] <- player.p[3]/denominator
        }
      }  
      if (validgames[games]==1)
      {
        cat (repetition,", ", sep="")
      }
    } # env.explore == "y"

    # ensure sum(player.p)==1
    if (numvertices==2)
    {
      denominator <- sum(player.p[1],player.p[2])
      player.p[1] <- player.p[1]/denominator
      player.p[2] <- player.p[2]/denominator
    }
    if (numvertices==3)
    {
      denominator <- sum(player.p[1],player.p[2],player.p[3])
      player.p[1] <- player.p[1]/denominator
      player.p[2] <- player.p[2]/denominator
      player.p[3] <- player.p[3]/denominator
    }
    # create data frame to store data
    # if it is unique (env.explore == "n")
    # if it is exploring and it is the first game
    if(
      env.explore == "n" |
      !(exists('df_evolution') && is.data.frame(get('df_evolution')))
    )
    {
      # to store data evolution
      df_evolution <- data.frame(matrix(ncol=length(player.name)+1, nrow=0))
      names(df_evolution) <- c("cycle",player.name)
    } 
    if (repetition == 1)
    {
      df_tmp <- data.frame(matrix(data=c(0,as.vector(player.p)),
                                  ncol=length(player.name)+1, nrow=1,
                                  byrow = TRUE
      ))
      names(df_tmp) <- c("cycle",player.name)
      df_evolution <- rbind(df_evolution,df_tmp)
    }
    if (env.explore == "y") # repeat to avoid spurious connections
    {
      df_tmp <- data.frame(matrix(data=c(0,as.vector(player.p)),
                                  ncol=length(player.name)+1, nrow=1,
                                  byrow = TRUE
      ))
      names(df_tmp) <- c("cycle",player.name)
      df_evolution <- rbind(df_evolution,df_tmp)
    }    
    arrow.len <- 0
    arrow.xy <- c()
    cycle <- 0
    while(1)
    {
      cycle <- cycle+1
      if ((cycle%%10000)==0) {cat(".")}
      # # liquid gain-loss
      liquid.matrix.tmp <- liquid.matrix
      for (target in player.name)
      {
        if (nchar(target) > 0 )
        {
          liquid.matrix.tmp[,target] <- liquid.matrix.tmp[,target] *
            player.p[which(colnames(player.p)==target)]
        }
      }
      delta.p <- player.p
      for (focal in player.name)
      {
        if (nchar(focal) > 0 )
        {
          delta.p[which(colnames(player.p)==focal)] <- w0 +
            sum(liquid.matrix.tmp[focal,])
        }
      }
      # new frequencies
      w_mean <- mean(delta.p[which(nchar(colnames(delta.p))>0)])
      player.p <- player.p*delta.p
      # normalize player.p
      player.p <- player.p/sum(player.p)
      # add to evolution
      df_tmp <- data.frame(matrix(data=c(cycle,as.vector(player.p)),
                                  ncol=length(player.name)+1, nrow=1,
                                  byrow = TRUE
      ))
      names(df_tmp) <- c("cycle",player.name)
      df_evolution <- rbind(df_evolution,df_tmp)
      l <- nrow(df_evolution)
      if (numvertices==2)
      {
        hdr <- c(    0,df_evolution[l-1,3],df_evolution[l-1,2])
        hdr <- c(hdr,0,df_evolution[l  ,3],df_evolution[l  ,2])
      }
      if (numvertices==3)
      {
        hdr <- c(    df_evolution[l-1,4],df_evolution[l-1,3],df_evolution[l-1,2])
        hdr <- c(hdr,df_evolution[l  ,4],df_evolution[l  ,3],df_evolution[l  ,2])
      }
      xy <- eiras.tern.coord(hdr)
      # hipotenusa: (dx^2 + dy^2) ^0.5
      step <- ( ((xy[2,1]-xy[1,1])^2) + ((xy[2,2]-xy[2,2])^2) )^0.5
      arrow.len <- arrow.len + step
      arrow.xy <- c(arrow.xy, xy[1,1], xy[1,2], xy[2,1], xy[2,2])
      col <- round((step*400)/0.2,0)
      if (col < 1) {col <- 1}
      if (col > 5) {col <- 5}
      if (cycle==1)
      {
        points(xy[1,1],xy[1,2],
               pch=21, cex=0.4, 
               col=friendlycolor(slow.to.fast[col]),
               bg=friendlycolor(slow.to.fast[col]))
      }
      lines(xy[,1],xy[,2], 
            col=friendlycolor(slow.to.fast[col]), lwd=1)
      # draw arrow  
      if ( (length(arrow.xy)%%draw.each)==0 )
      {
        l <- length(arrow.xy)-1 # last coordinates
        i <- l-2 
        d <- 0
        while (i>=1)
        {
          len <- ( ((arrow.xy[l  ]-arrow.xy[i])^2) + 
                     ((arrow.xy[l+1]-arrow.xy[i+1])^2) )^0.5
          if (len > stopif) {break}
          i <- i-2
        }
        if (len > stopif)
        {
          arrows(arrow.xy[i],arrow.xy[i+1],
                 arrow.xy[l],arrow.xy[l+1],
                 length = 0.1, angle = 20, 
                 col=friendlycolor(slow.to.fast[col]))
          d <- 1
        }
        if (arrow.len < stopif)
        {
          points(xy[2,1], xy[2,2], pch=21, 
                 col="black", bg="black", cex=0.8)
          break;
        }
        if (d==1)
        {
          arrow.xy <- c()
          arrow.len <- 0
        }
      }
    } # for cycle
    cat("(",cycle,"), ",sep="")
  } # for repetition
} # for games

cat("\nFinished:\n")

posread <- as.numeric(unlist(gregexpr(pattern=".", fileread, fixed=TRUE)))
if (posread[length(posread)] > 0)
{
  fileread <- substr(fileread,start=1,stop=posread[length(posread)]-1)
}
# save all computed trajectories
filewrite <- paste(fileread,"_results.xlsx",sep="")
cat("- trajectories stored in",filewrite,"\n")

wb <- openxlsx::createWorkbook("Results")
openxlsx::addWorksheet(wb,"Parameters")
openxlsx::writeData(wb, sheet="Parameters",df_config)
openxlsx::addWorksheet(wb,"Trajectories")
openxlsx::writeData(wb, sheet="Trajectories",df_evolution)
if(file.exists(filewrite)==TRUE)
{
  file.remove(filewrite)
}
openxlsx::saveWorkbook(wb,filewrite)

# gera a imagem do diagrama de fase
filewrite <- eiras.ternary.replot(filewrite,png="y")
cat("- plot image stored in",filewrite,"\n",sep="")

# reenable warnings
options(warn=oldw)

