midint<-function(data)
{
  if(!is.data.frame(data)) 
     stop("Invalid input parameter specification: check data")
  
  if(!(all(c("Start.Date","End.Date")%in%names(data))))
     stop("Error: data does not contain columns named Start.Date/End.Date")

 start<-tryCatch(as.POSIXct(as.character(data$Start.Date),tz="UTC"),error=function(e){return(NA)})
 end<-tryCatch(as.POSIXct(as.character(data$End.Date),tz="UTC"),error=function(e){return(NA)})
 
  if(is.na(start) || is.na(end))
     stop("Error: check dates format in data")
 
 start+time_length(interval(start,end))/2
 }
