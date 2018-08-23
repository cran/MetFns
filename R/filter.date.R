filter.date<-function(data,date.start,date.end)
{ 
  if(!is.data.frame(data))
     stop("Invalid input parameter(s) specification: check data")

  date1<-tryCatch(as.POSIXct(date.start,tz="UTC"),error=function(e){return(NA)})
  date2<-tryCatch(as.POSIXct(date.end,tz="UTC"),error=function(e){return(NA)})
 
  if(is.na(date1) || is.na(date2))
     stop("Invalid input parameter specification: check date.start/date.end format")
     
  if(date1>date2)
     stop("Error:date.end must be greater than date.start")
  
    
  mid.int<-midint(data)
  
  if(!(all(c(year(date1),year(date2))%in%unique(year(mid.int)))))
    stop("Error:years in data and date.start/date.end do not match")

  data[mid.int>=date1 & mid.int<=date2,]
  
  
}