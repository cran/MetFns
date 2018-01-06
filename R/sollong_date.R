sollong_date<-function(solval,year, date1=NULL,date2=NULL)
{
  if(!is.numeric(solval) || solval<0 || solval>360)
   stop("Invalid parameter specification: check value of solval")
   
  if(!is.numeric(year) || year<1984 || year>2030)
   stop("Invalid parameter specification: check value of year")
   
  beg<-paste(as.character(year),"-01-01", sep="") 
  
  if(!is.null(date1) && !is.null(date2)) {
   date1.new<-tryCatch(as.POSIXct(date1,tz="UTC"),error=function(e){return(NA)})
   date2.new<-tryCatch(as.POSIXct(date2,tz="UTC"),error=function(e){return(NA)})
  
   if(is.na(date1.new) || is.na(date2.new))
     stop("Invalid input parameter(s) specification: check date1 and date2 format")
 
   if(year(date1.new)!=year && year(date2.new)!=year)
   stop("Invalid parameter specification: years of date1 and date2 do not match")
  }else{
   date1.new<-as.POSIXct(beg,tz="UTC")
   date2.new<-as.POSIXct(paste(as.character(year),"-12-31", sep="") ,tz="UTC")
  }
  
   
  years<-1984:2030
   
  long0<-as.POSIXct(c("1984-03-20 04:49:50","1985-03-20 11:01:00","1986-03-20 17:11:48","1987-03-20 23:23:21",
           "1988-03-20 05:32:38","1989-03-20 11:44:41","1990-03-20 17:57:52","1991-03-21 00:02:01",
           "1992-03-20 06:08:59","1993-03-20 12:21:56","1994-03-20 18:28:33","1995-03-21 00:33:32",
           "1996-03-20 06:40:28","1997-03-20 12:50:00","1998-03-20 19:07:40","1999-03-21 01:17:08",
           "2000-03-20 07:25:25","2001-03-20 13:40:23","2002-03-20 19:45:51","2003-03-21 01:50:30",
           "2004-03-20 08:01:08","2005-03-20 14:08:09","2006-03-20 20:22:33","2007-03-21 02:26:52",
           "2008-03-20 08:30:21","2009-03-20 14:47:32","2010-03-20 20:57:14","2011-03-21 03:06:33",
           "2012-03-20 09:20:03","2013-03-20 15:26:41","2014-03-20 21:40:13","2015-03-21 03:46:11",
           "2016-03-20 09:49:25","2017-03-20 16:05:39","2018-03-20 22:10:41","2019-03-21 04:13:01",
           "2020-03-20 10:23:45","2021-03-20 16:32:02","2022-03-20 22:49:19","2023-03-21 05:02:03",
           "2024-03-20 11:06:41","2025-03-20 17:24:01","2026-03-20 23:30:42","2027-03-21 05:32:11",
           "2028-03-20 11:46:04","2029-03-20 17:51:47","2030-03-21 00:02:33"),tz="UTC")
           
  
  t0<-time_length(interval(as.POSIXct(beg,tz="UTC"),long0[year==years]))
  t1<-time_length(interval(as.POSIXct(beg,tz="UTC"),date1.new))
  t2<-time_length(interval(as.POSIXct(beg,tz="UTC"),date2.new))
  
  
  sol<-function(x){
     date_sollong(as.POSIXct(x,origin=beg,tz="UTC"),prec=5)-solval}    
     
  if(sol(t2)<0) t2<-t0-1
  if(sol(t1)>0) t1<-t0
  
  if(solval==0)
   {long0[year==years]
  }else{
  round_date(as.POSIXct(uniroot(sol,lower=t1, upper=t2)$root, origin =beg,tz="UTC"),unit="second") }
}









