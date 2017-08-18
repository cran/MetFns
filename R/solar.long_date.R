solar.long_date<-function(solval,date1,date2){
  as.POSIXct(mapply(sollong_date,solval,date1,date2),origin="1970-01-01",tz="UTC")
  
}