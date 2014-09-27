mag.distr<-function(data,year, month, day.beg, day.end=day.beg, shw)
{ 
    if(!is.data.frame(data) || !is.numeric(c(year,month,day.beg,day.end)) || day.beg>day.end)
        stop("invalid input parameter(s) specification")  

   data.shw<-filter(data,year,month,day.beg,day.end,shw)
   counts<-t(apply(data.shw[, which(names(data.shw)=="m6"):which(names(data.shw)=="p7")],2,sum))

   x<-rep(-6:7,counts)                          
   par(mfrow=c(1,2))
   hist(x,xlab="Magnitude",main="",right=F)
   boxplot(x)
}
