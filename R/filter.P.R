filter.P<-function(data,P.low=0,P.up=90)
{
   if(!is.data.frame(data)) 
      stop("Invalid input parameter specification: check data")
   
   if(!is.numeric(c(P.low,P.up)) || (P.low<0 || P.up>90) || P.low>P.up) 
      stop("Invalid input parameter(s) specification: check value(s) of P.low/P.up")
  
   if(!("P"%in%names(data)))
     stop("Error: data does not contain column named P")
     
   data[data$P>=P.low & data$P<=P.up,]
}
