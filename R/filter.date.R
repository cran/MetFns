filter.date<-function(data,date.start,date.end)
{ 
  if(!is.data.frame(data))
     stop("invalid input parameter(s) specification: check data")

  date1<-as.POSIXct(date.start,tz="UTC")
  date2<-as.POSIXct(date.end,tz="UTC")
  mid.int<-midint(data)

  data[mid.int>=date1 & mid.int<=date2,]
  
  
}