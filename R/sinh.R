sinh<-function(data,shw, Ralpha=NULL, Delta=NULL)
{
  data.shw<-filter.shw(data,shw)
  day.new<-day.mid(data.shw)[,2]
  midtime<-day.mid(data.shw)[,1]

  if(is.null(Ralpha) || is.null(Delta)){
    m<-nrow(data.shw)
    Ralpha<-rep(0,m)
    Delta<-rep(0,m)
    data(radiant,envir=environment())
    radiant<-get("radiant",envir=environment())
    i<-which(substr(names(radiant),start=1,stop=3)==shw)[1]
    
    for(j in 1:m){
        Ralpha[j]<-radiant[day.new[j]==radiant$Day&data.shw$month[j]==radiant$Month,i]
        Delta[j]<-radiant[day.new[j]==radiant$Day&data.shw$month[j]==radiant$Month,i+1] }
  }    
  if(any(is.na(Ralpha))) 
    stop("no table values for radiant coordinates")

  t<-hms2rad(ut2ha(yr=data.shw$year, mo=data.shw$month, dy=day.new, hr=midtime, ra.sou=paste(as.character(Ralpha*24/360),"h",sep=""), 
                    lon.obs=paste(data.shw$EW,paste(data.shw$long,"d",sep=""),sep=" ")))
  sine.h<-sin(dms2rad(Delta))*sin(dms2rad(data.shw$lat))+cos(dms2rad(Delta))*cos(dms2rad(data.shw$lat))*cos(t)
  data.frame(data.shw,sine.h=sine.h,day.new=day.new)
}
