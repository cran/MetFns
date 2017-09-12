zhr.plot<-function(data,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
{
 if(!is.data.frame(data)|| !("sollong"%in%names(data))|| !("ZHR"%in%names(data)) || !("st.err"%in%names(data)))
      stop("invalid input parameter(s) specification: check data")

 
 year<-year(data$date[1])
 solvals<-seq(xlim1,xlim2,by=xinc)
 dates<-solar.long_date(solvals,paste(year,"-01-01",sep=""),paste(year,"-12-31",sep=""))
 xlab2<-paste(day(dates),month.abb[month(dates)]," ",strftime(dates,format="%H:%M",tz="UTC"),sep="")
 
 par(mar=c(5,4,6,3))
 graph.data(data$sollong,data$ZHR,data$st.err,"ZHR(Corrected hourly meteor rate)",xlim1,xlim2,xinc,ylim1,ylim2,yinc)
 text(solvals,par("usr")[4],srt=45,xpd=TRUE,pos=4,offset=-0.1,labels=xlab2)
 mtext(paste("Time (UT,",year,")",sep=""),side=3,line=4,cex=1.2)
 
}
