dec.time<-function(t)
{
  if(!is.numeric(t) || t<0 || floor(t/100)>23 || 100*round((t/100)%%1,2)>59) 
     stop("invalid input parameter specification: check time")
  
  floor(t/100)+100*((t/100)%%1)/60
}
