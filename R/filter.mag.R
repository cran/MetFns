filter.mag<-function(data,mag.low=2.0,mag.up=7.5)
{
   if(!is.data.frame(data) || !is.numeric(c(mag.low,mag.up)) || (mag.low<2.0 || mag.up>7.5) || mag.low>mag.up) 
      stop("invalid input parameter(s) specification")
  
   data[data$lmg>=mag.low & data$lmg<=mag.up,]
}
