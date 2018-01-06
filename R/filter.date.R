filter.date<-function(data,date.start,date.end)
{ 
  if(!is.data.frame(data))
     stop("invalid input parameter(s) specification: check data")

  date1<-tryCatch(as.POSIXct(date.start,tz="UTC"),error=function(e){return(NA)})
  date2<-tryCatch(as.POSIXct(date.end,tz="UTC"),error=function(e){return(NA)})
 
  if(is.na(date1) || is.na(date2))
     stop("Invalid input parameter specification: check date start/date.end format")
     
  if(date1>date2)
     stop("Error:date.end must be greater than date.start")
    
     
  mid.int<-midint(data)

  data[mid.int>=date1 & mid.int<=date2,]
  
  
}