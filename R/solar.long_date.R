solar.long_date<-function(solval,year){
  as.POSIXct(mapply(sollong_date,solval,year),origin="1970-01-01",tz="UTC")
}