zhr.plot<-function(zhrdata,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
{
 if(!is.data.frame(zhrdata))
      stop("Invalid input parameter specification: check data")
      
 if(!("sollong"%in%names(zhrdata))|| !("ZHR"%in%names(zhrdata)) || !("st.error"%in%names(zhrdata)))
      stop("Error: zhrdata does not contain columns named sollong, ZHR and st.error")

 
 year<-year(zhrdata$date[1])
 solvals<-seq(xlim1,xlim2,by=xinc)
 dates<-solar.long_date(solvals,paste(year,"-01-01",sep=""),paste(year,"-12-31",sep=""))
 xlab2<-paste(day(dates),month.abb[month(dates)]," ",strftime(dates,format="%H:%M",tz="UTC"),sep="")
 
 par(mar=c(5,4,6,3))
 graph.data(zhrdata$sollong,zhrdata$ZHR,zhrdata$st.error,"ZHR(Corrected hourly meteor rate)",xlim1,xlim2,xinc,ylim1,ylim2,yinc)
 text(solvals,par("usr")[4],srt=45,xpd=TRUE,pos=4,offset=-0.1,labels=xlab2)
 mtext(paste("Time (UT,",year,")",sep=""),side=3,line=4,cex=1.2)
 
}
