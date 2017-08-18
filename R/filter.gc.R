filter.gc<-function(data,lat.low=-90,lat.up=90, long.low=-180,long.up=180)
{
   if(!is.data.frame(data) || !is.numeric(c(lat.low,lat.up,long.low,long.up)) || 
   lat.low<(-90) || long.low<(-180) || lat.up>90 || long.up>180 || lat.low>lat.up || long.low>long.up) 
      stop("invalid input parameter(s) specification: check data/lat.low/lat.up/long.low/long.up")
  
   data[data$Latitude>=lat.low & data$Latitude<=lat.up & data$Longitude>=long.low & data$Longitude<=long.up,]
}
