correct.F<-function(P)
{
 if(!is.numeric(P) || (P<0 || P>=100.0))
      stop("Invalid input parameter specification: check percentage P")

 1/(1-P)
 }