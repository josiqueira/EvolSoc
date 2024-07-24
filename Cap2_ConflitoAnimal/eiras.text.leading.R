# eiras.text.leading.R
# returns text with leading characters

text.leading <- function (text, lentxt, lead.chr=" ")
{
  text <- sprintf("%s",text)
  while(nchar(text) < lentxt)
  {
    text <- paste(lead.chr,text,sep="")  
  }
  return(text)
}
