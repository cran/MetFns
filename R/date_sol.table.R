date_sol.table<-function(date.beg,date.end,step)
{
  date1<-tryCatch(as.POSIXct(date.beg,tz="UTC"),error=function(e){return(NA)})
  date2<-tryCatch(as.POSIXct(date.end,tz="UTC"),error=function(e){return(NA)})

  if(is.na(date1) || is.na(date2))
     stop("Invalid input parameter specification: check date.beg/date.end format")
     
  if(date1>date2)
     stop("Error:date.end must be greater than date.beg")

  date1<-floor_date(date1,unit="day")
  date2<-floor_date(date2,unit="day")

  div<-c(5,10,15,20,30,60,120)

  if(!(step%in%div))
     stop("Error:choose different step")

  numcol<-1440/step
  dates<-seq(date1,date2+86400,by=60*step)
  n<-length(dates)
  colname<-format(dates[1:numcol],"%H:%M")
  rowname<-ymd(dates[seq(1,n-1,by=numcol)])
  sollongs<-solar.long(dates[-n])
  tablesol<-as.data.frame(matrix(sollongs,byrow=T,ncol=numcol))
  colnames(tablesol)<-colname
  rownames(tablesol)<-rowname
  tablesol
}
