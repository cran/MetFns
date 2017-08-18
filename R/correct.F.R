correct.F<-function(P)
{
 if(!is.numeric(P) || (P<0 || P>=100.0))
      stop("invalid input parameter(s) specification: check percent P")

 1/(1-P)
 }