sinh<-function(data,shw)
{
  data.shw<-filter.shw(data,shw)
  midtime<-midint(data.shw)
  ew<-rep("E",nrow(data.shw))
  ew[data.shw$Longitude<0]<-"W"  

  Ralpha<-Delta<-rep(NA,nrow(data.shw))
  data(radiant,envir=environment())
  radiant<-get("radiant",envir=environment())
  k<-which(substr(names(radiant),start=1,stop=3)==shw)[1]
  radiant.shw<-radiant[which(!is.na(radiant[,k])),c(1,2,k,k+1)]
    
   for(i in 1:nrow(radiant.shw)){
        ind<-month(midtime)%in%radiant.shw$Month[i] &day(midtime)%in%radiant.shw$Day[i]
        Ralpha[ind]<-radiant.shw[i,3]
        Delta[ind]<-radiant.shw[i,4] }
   
   indRD<-!is.na(Ralpha)
   data.shw<-data.shw[indRD,]
       
  
     
  t<-hms2rad(ut2ha(midtime[indRD], ra.sou=paste(as.character(Ralpha[indRD]*24/360),"h",sep=""), 
              lon.obs=paste(ew[indRD],paste(abs(data.shw$Longitude),"d",sep=""),sep=" ")))
  sine.h<-round(sin(dms2rad(Delta[indRD]))*sin(dms2rad(data.shw$Latitude))+cos(dms2rad(Delta[indRD]))*cos(dms2rad(data.shw$Latitude))*cos(t),3)
  cbind(data.shw,sine.h)
}


