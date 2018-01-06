sol_date.table<-function(solval,year){
  if(length(solval)>1 && length(year)>1)
   stop("Error: only one of the parameters can have length of 2 and greater.")
  dates<-data.frame(split(solar.long_date(solval,year),as.factor(solval)))
  colnames(dates)<-solval
  rownames(dates)<-year
  dates
}