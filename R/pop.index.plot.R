pop.index.plot<-function(rdata,xlim1,xlim2,xinc,ylim1,ylim2,yinc,dlim1=NULL,dlim2=NULL,dinc=NULL,dunit=NULL)
{
 if(!is.data.frame(rdata))
      stop("Invalid input parameter specification: check data")
 
 if(!("sollong"%in%names(rdata))|| !("pop.index"%in%names(rdata)) || !("r.error"%in%names(rdata)))
      stop("Error: rdata does not contain columns named sollong, pop.index, r.error")

  year<-year(rdata$date[1])
  
  if(is.null(dlim1) || is.null(dlim2) || is.null(dinc) || is.null(dunit)){
    solvals<-seq(xlim1,xlim2,by=xinc)
    dates<-solar.long_date(solvals,year)
  }else{
    date1<-tryCatch(as.POSIXct(dlim1,tz="UTC"),error=function(e){return(NA)})
    date2<-tryCatch(as.POSIXct(dlim2,tz="UTC"),error=function(e){return(NA)})
    
    if(is.na(date1) || is.na(date2))
      stop("Invalid input parameter specification: check dlim1/dlim2 format")
    
    if(date1>date2)
      stop("Error:dlim2 must be greater than dlim1")
    
    if(any(c(year(date1),year(date2))!=year))
      stop("Year in start and end dates have to match data year")
    
    if(!is.numeric(dinc))
      stop("Invalid input parameter specification: check dinc format")
    
    if(!dunit%in%c("min","h","day"))
      stop("Invalid input parameter specification: check dunit format")
    
    dstep<-function(dinc,dunit)
    {switch(dunit,min=60*dinc,h=3600*dinc,day=86400*dinc)
    }
    dates<-seq(date1,date2,by=dstep(dinc,dunit))
    solvals<-solar.long(dates)
    
  }
  
  if(!is.null(dunit) && dunit=="day")
  {xlab2<-paste(day(dates),month.abb[month(dates)],sep="")
  }else{
    xlab2<-paste(day(dates),month.abb[month(dates)]," ",strftime(dates,format="%H:%M",tz="UTC"),sep="")}
  
  ind<-!is.na(rdata$pop.index)
  
  par(mar=c(5,4,6,3))
  graph.data(rdata$sollong[ind],rdata$pop.index[ind],rdata$r.error[ind],"Population index",xlim1,xlim2,xinc,ylim1,ylim2,yinc)
  axis(3,solvals,labels=F,tcl=0.4)
  text(solvals,par("usr")[4]+0.1*yinc,srt=45,xpd=TRUE,pos=4,offset=-0.1,labels=xlab2)
  mtext(paste("Time (UT,",year,")",sep=""),side=3,line=4,cex=1.2)
}