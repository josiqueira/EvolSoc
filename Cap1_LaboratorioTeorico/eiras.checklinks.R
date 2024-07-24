# eiras.checklinks.R 

checklinks <- function(htmlpath)
{
  htmlpage <- data.table::fread(text=htmlpath, sep=NULL, header=FALSE)
  names(htmlpage) <- "linha"
  
  # destination folder
  # remove a extensao
  foldername <- paste0("Distribuicao_",tools::file_path_sans_ext(htmlpath))
  
  dir.create(foldername,showWarnings=FALSE)
  # remove arquivos se ja existem
  unlink(paste0(foldername,"/*"))
  
  # html
  file.copy(htmlpath,file.path(foldername,htmlpath),overwrite=TRUE)
  
  nameslinks <- c("link","kind","library","called from","depcheck")
  dt_links <- data.frame(matrix(nrow=0,ncol=length(nameslinks)))
  names(dt_links) <- nameslinks
  
  searchstr <- "<a href="
  for (r.aux in 1:nrow(htmlpage))
  {
    link <- grepl(searchstr,htmlpage$linha[r.aux])
    if(link)
    {
      posini <- unlist(gregexpr(searchstr, htmlpage$linha[r.aux]))
      for (p.aux in posini)
      {
        string <- substr(htmlpage$linha[r.aux],p.aux+nchar(searchstr)+1,nchar(htmlpage$linha[r.aux]))
        posnext <- unlist(gregexpr("\"", string))
        string <- substr(string,1,posnext[1]-1)
        # exclusions
        valid <- TRUE
        if (grepl("mailto:",string)) {valid <- FALSE}
        if (substr(string,1,1)=="#") {valid <- FALSE}
        if (substr(string,1,2)=="/a") {valid <- FALSE}
        if (valid)
        {
          # replace of spaces
          string <- gsub("%20"," ",string)
          cat("\n",r.aux,p.aux," --- ",string,"\n")
          kind <- ""
          if (grepl(".R",string)) {kind <- "script"}
          if (grepl(".xls",string)) {kind <- "file"}
          if (grepl(".txt",string)) {kind <- "file"}
          if (substr(string,1,4)=="http") {kind <- "web"}
          if(nchar(kind)>0)
          {
            dt_tmp <- data.frame(string,kind,"","html",0)
            names(dt_tmp) <- nameslinks
            dt_links <- rbind(dt_links,dt_tmp)
          }
        }
      }
    }
  }
  
  # adiciona dependencias dos scripts
  dt_links$depcheck <- 0
  for (r.aux in 1:nrow(dt_links))
  {
    if(dt_links$kind[r.aux]=="script")
    {
      dt_links$depcheck[r.aux] <- 1
    }
  }
  
  add <- TRUE
  while (add)
  {
    add <- FALSE
    for (r.aux in 1:nrow(dt_links))
    {
      if(dt_links$kind[r.aux]=="script" & dt_links$depcheck[r.aux]==1)
      {
        dt_rscript <- data.table::fread(text=dt_links$link[r.aux], sep=NULL, header=FALSE)
        names(dt_rscript) <- "linha"
        for (r2.aux in 1:nrow(dt_rscript))
        {
          # fontes chamados de fontes
          searchstrs <- c("source","library")
          for (searchstr in searchstrs)
          {
            link <- grepl(searchstr,dt_rscript$linha[r2.aux])
            ok <- FALSE
            if(link)
            {
              posini <- unlist(gregexpr(searchstr, dt_rscript$linha[r2.aux]))
              string <- substr(dt_rscript$linha[r2.aux],posini+nchar(searchstr),nchar(dt_rscript$linha[r2.aux]))
              posini <- unlist(gregexpr("\\(", string)) # avanca para abre aspas
              posnext.fechapar <- unlist(gregexpr(")", string))[1]
              posnext.virgula <- unlist(gregexpr(",", string))[1]
              if(posnext.fechapar>=1 & posnext.virgula>=1)
              {
                # ambas existem, pega o mais proximo
                posnext <- min(posnext.fechapar,posnext.virgula)
              } else
              {
                if(posnext.fechapar>=1){posnext <- posnext.fechapar}
                if(posnext.virgula>=1){posnext <- posnext.virgula}
              }
              string <- substr(string,posini+1,posnext-1)
              # elimina aspas, se houver
              string <- gsub("\"","",string)
              if (searchstr=="source")
              {
                kind <- "script"
              }
              if (searchstr=="library")
              {
                kind <- "library"
              }
              addrow <- which(dt_links$link==string & dt_links$kind==kind)
              if (length(addrow)==0) # new occurrence
              {
                dt_tmp <- data.frame(string,kind,"","script",0)
                names(dt_tmp) <- nameslinks
                if(kind=="script"){dt_tmp$depcheck<-1}
                dt_links <- rbind(dt_links,dt_tmp)
                add <- TRUE
                cat("\n",string)
              }
            } # fontes
          }  
          # ocorrencias de :: (libraries dentro do codigo)
          searchstr <- "::"
          link <- grepl(searchstr,dt_rscript$linha[r2.aux])
          if(link)
          {
            posini <- unlist(gregexpr(searchstr, dt_rscript$linha[r2.aux]))
            # recua ate o espaco em branco que antecede. ( ou =)
            for(p2.aux in posini)
            {
              p.aux <- p2.aux
              ok <- FALSE
              while (!ok) 
              {
                p.aux <- p.aux-1
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)==" ") {ok<-TRUE}
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)=="(") {ok<-TRUE}
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)=="=") {ok<-TRUE}
                if (p.aux==0) {ok<-TRUE}
              }
              string <- substr(dt_rscript$linha[r2.aux],p.aux+1,posini-1)
              kind <- "library::"
              addrow <- which(dt_links$link==string & dt_links$kind==kind)
              if (length(addrow)==0) # new occurrence
              {
                dt_tmp <- data.frame(string,"library","",kind,0)
                names(dt_tmp) <- nameslinks
                dt_links <- rbind(dt_links,dt_tmp)
                add <- TRUE
                cat("\n",string)
              }
              # avança ate o espaco em branco ou ( que sucede
              p.aux <- p2.aux+1
              ok <- FALSE
              while (!ok) 
              {
                p.aux <- p.aux+1
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)==" ") {ok<-TRUE}
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)=="(") {ok<-TRUE}
                if (p.aux==nchar(dt_rscript$linha[r2.aux])) {ok<-TRUE}
              }
              string <- substr(dt_rscript$linha[r2.aux],posini+2,p.aux-1)
              kind <- "::function"
              # antecedente, nome da library, recua ate espaco ou (
              p.aux <- p2.aux+1
              ok <- FALSE
              while (!ok) 
              {
                p.aux <- p.aux-1
                if (p.aux==0) {ok <- TRUE} # inicio da linha, sem nada antes
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)==" ") {ok<-TRUE}
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)=="(") {ok<-TRUE}
                if (p.aux==nchar(dt_rscript$linha[r2.aux])) {ok<-TRUE}
              }
              libname <- substr(dt_rscript$linha[r2.aux],p.aux+1,posini-1)
              addrow <- which(dt_links$link==string & dt_links$kind==kind)
              if (length(addrow)==0) # new occurrence
              {
                dt_tmp <- data.frame(string,"function",libname,kind,0) #@@
                names(dt_tmp) <- nameslinks
                dt_links <- rbind(dt_links,dt_tmp)
                add <- TRUE
                cat("\n",string)
              }
            }
            
          } # library
          # ocorrencias de files dentro de scripts
          searchstrs <- c(".xls\"",".xlsx\"",".csv\"",".txt\"",".sav\"",".dta\"")
          for (searchstr in searchstrs)
          {
            link <- grepl(searchstr,dt_rscript$linha[r2.aux])
            if(link)
            {
              posini <- unlist(gregexpr(searchstr, dt_rscript$linha[r2.aux]))
              # recua ate as aspas que antecedem
              p.aux <- posini
              ok <- FALSE
              while (!ok) 
              {
                p.aux <- p.aux-1
                if (substr(dt_rscript$linha[r2.aux],p.aux,p.aux)=="\"") {ok<-TRUE}
                if (p.aux==0) {ok<-TRUE}
              }
              string <- paste0(substr(dt_rscript$linha[r2.aux],p.aux+1,posini-1),
                               substr(searchstr,1,nchar(searchstr)-1) )
              kind <- "script"
              addrow <- which(dt_links$link==string & dt_links$kind==kind)
              if (length(addrow)==0) # new occurrence
              {
                dt_tmp <- data.frame(string,"file","",kind,0)
                names(dt_tmp) <- nameslinks
                dt_links <- rbind(dt_links,dt_tmp)
                add <- TRUE
                cat("\n",string)
              }
            } # files
          } # for searchstr
          
        }
        dt_links$depcheck[r.aux] <- 0
      }
    }
  }
  
  # remove duplicatas
  dt_links <- dt_links[order(dt_links$link),]
  dt_links$duplicate <- 0
  for (r.aux in 2:nrow(dt_links))
  {
    if (
      dt_links$link[r.aux]==dt_links$link[r.aux-1] &
      dt_links$`called from`[r.aux]==dt_links$`called from`[r.aux-1]
    )
    {
      dt_links$duplicate[r.aux] <- 1
    }
  }
  dt_links <- dt_links[dt_links$duplicate==0,]
  dt_links$duplicate <- NULL
  
  # Coloca tudo que precisa na pasta de Distribuicao
  for (r.aux in 1:nrow(dt_links))
  {
    if(dt_links$kind[r.aux]=="web") {next}
    if(dt_links$kind[r.aux]=="library") {next}
    
    file.copy(dt_links$link[r.aux],file.path(foldername,dt_links$link[r.aux]),overwrite=TRUE)
  }
  
  dt_links$depcheck <- NULL
  dt_links <- dt_links[order(dt_links$kind,dt_links$link),]
  # remove a extensao
  filename <- paste0(tools::file_path_sans_ext(htmlpath),"_dependences.xlsx")
  # planilha com nome igual ao html
  filelink <- file.path(foldername, filename)
  openxlsx::write.xlsx(dt_links,filelink)
  
  dt_calls <- dt_links
  dt_calls$`called from` <- NULL
  dt_calls <- na.omit(dt_calls)
  dt_calls <- dt_calls[nchar(dt_calls$link)>0,]
  dt_calls <- dt_calls[order(dt_calls$link,dt_calls$kind),]
  kinds <- c("library","script","file")
  
  outtxt <- "\n# Pacotes, funções e arquivos necessários"
  # libraries
  k <- 1
  dt_tmp <- dt_calls[dt_calls$kind==kinds[k],]  
  strings <- unique(dt_tmp$link)
  outtxt <- paste0(outtxt,"\n\n* packages","\n")
  outtxt <- paste0(outtxt,"```{r, echo=TRUE, eval=FALSE, class.source=\"bgcodigo\", class.output=\"bgsaida\"}","\n")
  outtxt <- paste0(outtxt,"options(warn=-1)","\n")
  for (i in 1:length(strings))
  {
    outtxt <- paste0(outtxt,"suppressMessages(library(",strings[i],", warn.conflicts=FALSE))","\n")
  }
  outtxt <- paste0(outtxt,"options(warn=0)","\n")
  outtxt <- paste0(outtxt,"```","\n")
  
  # scripts
  k <- 2
  dt_tmp <- dt_calls[dt_calls$kind==kinds[k],]  
  strings <- unique(dt_tmp$link)
  outtxt <- paste0(outtxt,"\n\n* scripts","\n")
  for (i in 1:length(strings))
  {
    outtxt <- paste0(outtxt,"\n    * [",strings[i],"](",strings[i],")")
  }
  outtxt <- paste0(outtxt,"\n")
  
  # files
  k <- 3
  dt_tmp <- dt_calls[dt_calls$kind==kinds[k],]  
  strings <- unique(dt_tmp$link)
  outtxt <- paste0(outtxt,"\n\n* files","\n")
  for (i in 1:length(strings))
  {
    outtxt <- paste0(outtxt,"\n    * [",strings[i],"](",strings[i],")")
  }
  outtxt <- paste0(outtxt,"\n")
  
  # remove a extensao
  filename <- paste0(tools::file_path_sans_ext(htmlpath),"_dependences.txt")
  # arquivo texto com nome igual ao html
  filelink <- file.path(foldername, filename)
  sink(filelink)
  cat(outtxt)
  sink()
  
  cat(outtxt)
  
  cat(paste0("\n\n---------\nDistribuicao disponivel na pasta ",foldername,"\n"))
  cat(paste0("Veja as dependencias em ",filename," (para copiar ao Rmd)","\n"))
}

