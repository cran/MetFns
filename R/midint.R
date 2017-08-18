midint<-function(data)
{
  if(!is.data.frame(data)) 
     stop("invalid input parameter specification: check data")

 start<-as.POSIXct(as.character(data$Start.Date),tz="UTC")
 end<-as.POSIXct(as.character(data$End.Date),tz="UTC")
 start+time_length(interval(start,end))/2
 }
