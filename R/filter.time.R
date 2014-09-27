filter.time<-function(data,time.low=0,time.up=2359)
{
   if(!is.data.frame(data) || !is.numeric(c(time.low,time.up)) || (time.low<0 || time.up>2359)) 
      stop("invalid input parameter(s) specification")
  
   if(time.low<time.up)
      ind<-data$start<time.up&data$stop>time.low
   else
      ind<-1
   
   data[data$start>=time.low & data$stop<=time.up & ind,]
}
