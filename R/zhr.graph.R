zhr.graph<-function(data,year,month, day.beg, day.end=day.beg,shw, r=NULL, Ralpha=NULL, Delta=NULL, k,c=1,type=c("UTC","sol"))
{
 data.zhr<-zhr(data,year,month, day.beg, day.end,shw, r, Ralpha, Delta, k,c)
 x<-seq(k/2,by=k,length=(day.end-day.beg+1)*(24/k))
 xTick<-seq(0,by=k,length=(day.end-day.beg+1)*(24/k)+1)
 xtext<-ifelse(type=="UTC","Time(UTC)","Solar longitude (J2000.0)")
 
 ymax<- max(apply(data.zhr[,9:10],1,sum),na.rm=T)
 ind<-ifelse(ymax<75,1,2)
 ind<-ifelse(ymax<110,ind,4)
 yTick<-seq(0,ymax+5*ind,by=5*ind)

 par(mfrow=c(1,1))
 plotCI(x,data.zhr$ZHR,uiw=data.zhr$st.err, pch=20,xlab=xtext,
        ylab="ZHR (Corrected hourly meteor rate)", main="",xaxt="n",yaxt="n",
        xaxs="i", yaxs="i", xlim=c(0,max(xTick)),ylim=c(0,max(yTick)))
 axis(1,xTick,labels=F,tcl=0.2)
 axis(2,yTick,tcl=0.2)
 dd<-day.beg:(day.end+1)
 sol<-Vectorize(solar.long)
 if(type=="UTC")
   {xlabels<-paste(month.abb[month],dd,sep=" ")}
 else 
   {xlabels<-round(sol(year,month,dd,0),1)}

 day<-xTick[seq(1,length(xTick),by=24/k)]
 axis(1,day,labels=xlabels,tcl=0.2)
 abline(v=day,h=yTick,lty=3,col="gray")
}
