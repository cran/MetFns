mag.distr<-function(data,date.start,date.end,shw)
{ 

   data.shw<-filter(data,date.start,date.end,shw)
   counts<-t(apply(data.shw[, which(names(data.shw)=="Mag.N6"):which(names(data.shw)=="Mag.7")],2,sum))
   
   x<-rep(-6:7,counts)                          
   par(mfrow=c(1,2))
   hist(x,breaks=seq(-6.5,7.5,by=1),xlab="Magnitude",main="",right=F,xaxt="n")
   axis(1,-6:7,pos=0)
   boxplot(x)
   counts
}
