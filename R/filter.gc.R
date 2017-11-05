filter.gc<-function(data,lat.low=-90,lat.up=90, long.low=-180,long.up=180)
{
   if(!is.data.frame(data)) 
      stop("Invalid input parameter specification: check data")
   
   if(!is.numeric(c(lat.low,lat.up,long.low,long.up)) || lat.low<(-90) || long.low<(-180) 
      || lat.up>90 || long.up>180 || lat.low>lat.up || long.low>long.up) 
      stop("Invalid input parameter(s) specification: check value(s) of lat.low/lat.up/long.low/long.up")
   
   if(!(all(c("Latitude","Longitude")%in%names(data))))
     stop("Error: data does not contain columns named Latitude/Longitude")
  
   data[data$Latitude>=lat.low & data$Latitude<=lat.up & data$Longitude>=long.low & data$Longitude<=long.up,]
}
