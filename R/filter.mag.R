filter.mag<-function(data,mag.low=0,mag.up=8)
{
   if(!is.data.frame(data) || !is.numeric(c(mag.low,mag.up)) || mag.up>8 ||  mag.low>mag.up) 
      stop("invalid input parameter(s) specification: check data/mag.low/mag.up")
  
   data[data$Lmg>=mag.low & data$Lmg<=mag.up,]
}
