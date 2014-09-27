dec.time<-function(t)
{
  if(!is.numeric(t)) 
     stop("invalid input parameter specification")
  
  floor(t/100)+100*((t/100)%%1)/60
}
