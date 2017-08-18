filter.P<-function(data,P.low=0,P.up=20)
{
   if(!is.data.frame(data) || !is.numeric(c(P.low,P.up)) || (P.low<0 || P.up>=100.0) || P.low>P.up) 
      stop("invalid input parameter(s) specification: check data/P.low/P.up")
  
   data[data$P>=P.low & data$P<=P.up,]
}
