mag.distr<-function(data,date.start,date.end,shw)
{ 
   if(!(all(c("Mag.N6","Mag.N5","Mag.N4","Mag.N3","Mag.N2","Mag.N1","Mag.0","Mag.1","Mag.2","Mag.3", 
              "Mag.4","Mag.5","Mag.6","Mag.7")%in%names(data))))
     stop("Error: data does not contain columns named Mag.N6, Mag.N5, Mag.N4, Mag.N3, Mag.N2, Mag.N1, 
           Mag.0, Mag.1,Mag.2, Mag.3, Mag.4, Mag.5, Mag.6 and Mag.7")

   data.shw<-filter(data,date.start,date.end,shw)
   counts<-t(apply(data.shw[, which(names(data.shw)=="Mag.N6"):which(names(data.shw)=="Mag.7")],2,sum))
   
   x<-rep(-6:7,counts)                          
   par(mfrow=c(1,2))
   hist(x,breaks=seq(-6.5,7.5,by=1),xlab="Magnitude",main="",right=F,xaxt="n")
   axis(1,-6:7,pos=0)
   boxplot(x)
   counts
}
