filter.mag<-function(data,mag.low=1,mag.up=8)
{
   if(!is.data.frame(data)) 
      stop("Invalid input parameter specification: check data")
      
   if(!is.numeric(c(mag.low,mag.up)) || mag.low<1 || mag.up>8 ||  mag.low>mag.up) 
      stop("Invalid input parameter(s) specification: check value(s) of mag.low/mag.up")
   
   if(!("Lmg"%in%names(data)))
     stop("Error: data does not contain column named Lmg")
  
  
   data[data$Lmg>=mag.low & data$Lmg<=mag.up,]
}
