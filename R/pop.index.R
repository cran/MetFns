pop.index<-function(data,year, month, day.beg, day.end=day.beg, shw, mag=-6:7)
{ 
   if(!is.data.frame(data) || !is.numeric(c(year,month,day.beg,day.end)) || day.beg>day.end || length(mag)<5)
      stop("invalid input parameter(s) specification")  
     
   data.shw<-filter(data,year,month,day.beg,day.end,shw)
   
   
   counts<-t(apply(data.shw[, which(names(data.shw)=="m6"):which(names(data.shw)=="p7")],2,sum))
   cum.freq<-cumsum(counts)
   
   mag.val<--6:7
   ind<-mag.val %in% mag & (!counts %in% 0:2) & mag.val<=5

   mag<-mag.val[ind]
   cum.freq<-cum.freq[ind]
   mag0.ind<-match(0,mag)
   y<-log(cum.freq/cum.freq[mag0.ind])

   a_est<-sum(mag*y)/sum(mag^2)
   pop.index<-exp(a_est)

   var.est<-1/(length(mag)-2)*sum((y-a_est*mag)^2)
   var.a<-var.est/sum(mag^2)
   sigma.r<-pop.index*sqrt(var.a)
   
   data.frame(Day=paste(day.beg, "-", day.end,sep=""),Month=month,Year=year, mag=paste(mag[1],":",mag[length(mag)],sep=""),
              nINT=nrow(data.shw),nSHW=sum(data.shw[,which(names(data.shw)=="N")]),pop.index=round(pop.index,2),sigma.r=round(sigma.r,2))
 
}
