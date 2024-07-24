# eiras.jitter.R
# jitter = c(0, -1, num)
# respectively: none, auto or given value

jitter <- function (values, jitter=NA, l.lim=NA, u.lim=NA)
{
  if (is.na(jitter))
  {
    iq <- quantile(values, probs=c(0.25,0.75), na.rm=TRUE)
    jitter <- abs(iq[2]-iq[1]+runif(1,0,abs(mean(iq))))/20
  }
  if (jitter > 0)
  {
    values <- values + runif(length(values),-jitter,jitter)
    if (!is.na(l.lim))
    {
      for (v in 1:length(values))
      {
        if (values[v]<l.lim) {values[v]<-l.lim}
      }
    }
    if (!is.na(u.lim))
    {
      for (v in 1:length(values))
      {
        if (values[v]>u.lim) {values[v]<-u.lim}
      }
    }
  }
  
  return(values)
}
