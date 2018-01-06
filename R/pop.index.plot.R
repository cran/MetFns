pop.index.plot<-function(rdata,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
{
 if(!is.data.frame(rdata))
      stop("Invalid input parameter specification: check data")
 
 if(!("sollong"%in%names(rdata))|| !("pop.index"%in%names(rdata)) || !("r.error"%in%names(rdata)))
      stop("Error: rdata does not contain columns named sollong, pop.index, r.error")

  year<-year(rdata$date[1])
  solvals<-seq(xlim1,xlim2,by=xinc)
  dates<-solar.long_date(solvals,year)
  xlab2<-paste(day(dates),month.abb[month(dates)]," ",strftime(dates,format="%H:%M",tz="UTC"),sep="")
  ind<-!is.na(rdata$pop.index)
  
  par(mar=c(5,4,6,3))
  graph.data(rdata$sollong[ind],rdata$pop.index[ind],rdata$r.error[ind],"Population index",xlim1,xlim2,xinc,ylim1,ylim2,yinc)
  text(solvals,par("usr")[4],srt=45,xpd=TRUE,pos=4,offset=-0.1,labels=xlab2)
  mtext(paste("Time (UT,",year,")",sep=""),side=3,line=4,cex=1.2)
}