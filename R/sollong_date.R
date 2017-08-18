sollong_date<-function(solval,date1,date2,unit="minute")
{
  date1.new<-as.POSIXct(date1,tz="UTC")
  date2.new<-as.POSIXct(date2,tz="UTC")
  if(year(date1.new)!=year(date2.new))
   stop("invalid parameter specification: years of date1 and date2 do not match")
   
  if(!(unit%in%c("second","minute"))) 
   stop("invalid parameter specification: check value of unit")
   
  if(solval<0 || solval>=360)
   stop("invalid parameter specification: check value of solval")
  
  
  year<-year(date1.new)
  beg<-paste(as.character(year),"-01-01", sep="") 
  
  t1<-time_length(interval(as.POSIXct(beg,tz="UTC"),date1.new))
  t2<-time_length(interval(as.POSIXct(beg,tz="UTC"),date2.new))
  
  
  sol<-function(x){
     date_sollong(as.POSIXct(x,origin=beg,tz="UTC"),prec=11)-solval}


  years<-1984:2020
  seconds<-c(6842968,6778874,6801102,6823413,6845581,6781487,6803852,6825734,6847739,
             6783728,6805704,6827620,6849624,6785412,6808052,6830236,6852326,6788427,
             6810352,6832263,6854459,6790090,6812542,6834409,6856219,6792465,6814622,
             6836793,6859193,6794787,6817197,6839168,6860952,6797152,6819026,6840779,6863038)
  if(sol(t2)<0) t2<-seconds[year==years]
  if(sol(t1)>0) t1<-seconds[year==years]+1
  
  if(solval==0)
  {round_date(as.POSIXct(seconds[year==years]+1,origin =beg,tz="UTC"),unit)}
  else{
  round_date(as.POSIXct(uniroot(sol,lower=t1, upper=t2)$root, origin =beg,tz="UTC"),unit) }
}









