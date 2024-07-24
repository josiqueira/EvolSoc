# eiras.ternary.replot.R

options(warn=-1)

library(readxl)

source ("eiras.rgb2rgbstring.R")
source ("eiras.redblue.gradation.R")
source("eiras.friendlycolor.R")
source("eiras.exit.R")
source("eiras.tern.graph.R")

# filename <- "RetaliatorHawkAssessor_results.xlsx"
# plotcolor=FALSE
# background=FALSE
# png="n"

eiras.ternary.replot <- function(filename, plotcolor=NA, background=NA, png="n")
{
  df_cfg <- read_excel(filename, sheet="Parameters")
  df_res <- read_excel(filename, sheet="Trajectories")
  cat("\nFound: ",filename,"\n",sep="")
  
  # parameters
  txt <- as.character(df_cfg[which(
    df_cfg[,1]=="w0"),1:2])
  w0 <- as.numeric(txt[2])
  txt <- as.character(df_cfg[which(
    df_cfg[,1]=="v"),1:2])
  v <- as.numeric(txt[2])
  txt <- as.character(df_cfg[which(
    df_cfg[,1]=="c"),1:2])
  c <- as.numeric(txt[2])
  txt <- as.character(df_cfg[which(
    df_cfg[,1]=="x"),1:2])
  x <- as.numeric(txt[2])
  if(is.na(x)) {x <- 0}
  kind <- c(NA,NA)
  if (x>0)
  {
    txt <- as.character(df_cfg[which(
      df_cfg[,1]=="kind[1]"),1:2])
    kind[1] <- as.character(txt[2])
    txt <- as.character(df_cfg[which(
      df_cfg[,1]=="kind[2]"),1:2])
    kind[2] <- as.character(txt[2])
  }

  if (is.na(plotcolor))
  {
    txt <- as.character(df_cfg[which(
      df_cfg[,1]=="Color"),1:3])
    env.color <- as.character(txt[2])
    if (env.color == "y") 
    {
      cat("- showing colored graphs\n",sep="")
      plotcolor <- TRUE
    }
    if (env.color == "n") 
    {
      cat("- showing grayed graphs\n",sep="")
      plotcolor <- FALSE
    }
  }
  
  if (is.na(background))
  {
    txt <- as.character(df_cfg[which(
      df_cfg[,1]=="Background"),1:3])
    env.bg <- as.character(txt[2])
    if (env.bg == "y") 
    {
      cat("- showing background\n",sep="")
      background <- "#d8d8d8"
    }
    if (env.bg == "n") 
    {
      cat("- showing background\n",sep="")
      background <- "#ffffff"
    }
  }
    
  filewrite <- NA
  if (png=="y")
  {
    # save image
    filewrite <- paste(filename,".png",sep="")
    png(filename = filewrite, width = 500*1.154701, height = 500)
  } 


  # player names
  player.name <- names(df_res)[2:ncol(df_res)]
  player.name[player.name=="NA"] <- NA
  player.name <- player.name[!is.na(player.name)]
  numvertices <- length(player.name)
  df_res$x <- NA
  df_res$y <- NA
  df_res$speed <- NA
  l.step <- 0.001 # segment size
  cat("\nProcessing points: ",sep="")
  r <- 1
  hdr <- as.vector(as.numeric(df_res[r,(numvertices+1):2])) 
  if (numvertices==2)
  {
    hdr <- c(0,hdr)
  }
  oldxy <- eiras.tern.coord(hdr);
  df_res$x[r] <- oldxy[1,1];
  df_res$y[r] <- oldxy[1,2];
  df_res$speed[r] <- NA
  for (r in 2:nrow(df_res))
  {
    if ((r%%1000)==0)
    {
      cat(r,", ", sep="")
    }
    hdr <- as.vector(as.numeric(df_res[r,(numvertices+1):2])) 
    if (numvertices==2)
    {
      hdr <- c(0,hdr)
    }
    xy <- eiras.tern.coord(hdr);
    df_res$x[r] <- xy[1,1];
    df_res$y[r] <- xy[1,2];
    if (df_res$cycle[r]==0)
    {
      # ponto inicial
      df_res$speed[r] <- NA
    }
    else
    {
      df_res$speed[r] <-  (
        ((df_res$x[r]-df_res$x[r-1])^2)+
          ((df_res$y[r]-df_res$y[r-1])^2))^0.5
    }
  }
  cat("\n")

  # shift to positive numbers
  df_res$transformation <- NA
  df_res$transformation <- df_res$speed
  ok <- 0
  while(ok==0)
  {
    d <- density(df_res$transformation,na.rm = TRUE)
    mintk <- min(d$x)
    df_res$transformation <- df_res$transformation+abs(mintk)
    d <- density(df_res$transformation,na.rm = TRUE)
    if (sum(d$x<0)==0){ok <- 1}
  }
  
  # colors
  levels <- 30
  ck <- seq(0,1,length.out=levels)
  qk <- quantile(df_res$transformation, probs=ck, na.rm = TRUE)
  df_k <- data.frame(ck,as.numeric(qk))
  names(df_k) <- c("ck","qk")
  df_k$color <- NA
  max.k <- df_k$qk[nrow(df_k)]
  df_k$ck <- (-1/log(df_k$ck))/(max.k)
  df_k <- df_k[1:(nrow(df_k)-1),]
  df_k$ck <- df_k$ck/max(df_k$ck)
  for (k in 1:(nrow(df_k)))
  {
    df_k$color[k] <- eiras.redblue.gradation(df_k$ck[k], color=plotcolor)
  }
  df_res$color <- NA
  df_res$transformation[df_res$transformation<0] <- 0
  for (k in 1:nrow(df_k) )
  {
    df_res$color[df_res$transformation>df_k$qk[k]] <- df_k$color[k]
  }
  if (png=="n")
  {
    Sys.sleep(3)
  }
  # fill cycle==0
  df_res$cycle[is.na(df_res$cycle)] <- 0
  for(r in nrow(df_res):1)
  {
    if(df_res$cycle[r]!=0) {col <- df_res$color[r]}
    if(df_res$cycle[r]==0) { df_res$color[r]<-col } 
  }
  
  # open graph
  grphtitle <- paste("w0=",w0,", v=",v,", c=",c,sep="")
  if (x>0)
  {
    grphtitle <- paste(grphtitle,", x=",x,", p.win[",kind[1],"]=",x,sep="")
  }
  player.name <- names(df_res)[2:(numvertices+1)]
  eiras.tern.graph(
    main=grphtitle,
    label.left = player.name[1],
    label.right = player.name[2],
    label.top = player.name[3],
    g.bg = background,  
  )
  cat("\nPlotting: ")
  if (png=="n")
  {
    Sys.sleep(3)
  }
  ucol <- sort(unique(df_res$color))
  # for (u in 1:length(ucol))
  # {
  #   df_plot <- df_res[df_res$color==ucol[u],]
  #   points(df_plot$x,df_plot$y,
  #          cex=0.1,pch=21,col=ucol[u],bg=ucol[u])
  # }

  # pontos iniciais
  df_plot <- df_res[df_res$cycle==0,]
  for (u in 1:length(ucol))
  {
    points(df_plot$x[df_plot$color==ucol[u]],
           df_plot$y[df_plot$color==ucol[u]],
           cex=0.6,pch=21,col=ucol[u],bg=ucol[u])
  }
  
  # pontos finais
  df_plot <- df_res[nrow(df_res),]
  for (r in 1:(nrow(df_res)-1))
  {
    if (df_res$cycle[r]!=0 & df_res$cycle[r+1]==0)
    {
      df_tmp <- df_res[r,]
      df_plot <- rbind(df_plot,df_tmp)
    }
  }
  points(df_plot$x,df_plot$y,
         cex=1.3,pch=21,col="gray",bg="black")
  if (png=="n")
  {
    Sys.sleep(0.5)
  }
  cycles<-df_res$cycle
  c2 <- cycles[2:length(cycles)]
  c2 <- c(c2,0)
  c3 <- c2-cycles
  stop <- which(c3<0)
  stop <- c(1,stop)
  for (s in 2:length(stop))
  {
    df_res$x[stop[s]] <- NA
    df_res$y[stop[s]] <- NA
  }
  for (s in 2:length(stop))
  {
    cat(".")
    r <- stop[s-1]:stop[s]
    for (l in 1:(length(r)-1))
    {
      lines(
        df_res$x[r[l]:r[l+1]],
        df_res$y[r[l]:r[l+1]],
        col=df_res$color[r[l]]
      )
      # arrows
      if ( (l%%30)== 0)
      {
        dx <- df_res$x[r[l]]-df_res$x[r[l+1]]
        dy <- df_res$y[r[l]]-df_res$y[r[l+1]]
        if (!is.na(dx) & !is.na(dy))
        {
          hipotenusa <- (dx^2+dy^2)^0.5
          if (dx!=0)
          {
            beta1 <- dy/dx
            beta0 <- mean (df_res$y[r[l]]-df_res$y[r[l+1]]) -
              beta1 * mean (df_res$x[r[l]]-df_res$x[r[l+1]])
          } 
          dirx <- 0
          if (dx>0) {dirx <-  1}
          if (dx<0) {dirx <- -1}
          diry <- 0
          if (dy>0) {diry <-  1}
          if (dy<0) {diry <- -1}
          if ((dx!=0 | dy!=0 ) & hipotenusa>1e-3) 
          {
            arrows(df_res$x[r[l]],df_res$y[r[l]],
                   df_res$x[r[l+1]],df_res$y[r[l+1]],
                   col=df_res$color[r[l]],
                   length=0.08, angle=20
            )
          }
          if ( (l%%300)== 0)
          {
            if (png=="n")
            {
              Sys.sleep(0.25)
            }
          }
        }
      }
    }
    # lines(
    #   df_res$x[stop[s-1]:stop[s]],
    #   df_res$y[stop[s-1]:stop[s]]
    # )
  }
  cat("\n")
  
  if (png=="y")
  {
    dev.off()
  }
  
  cat("\nFinished\n")
  
  return(filewrite)
}

# options(warn=0)
