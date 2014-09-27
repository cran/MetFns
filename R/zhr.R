zhr<-function(data,year, month, day.beg,day.end=day.beg,shw, r=NULL, Ralpha=NULL, Delta=NULL, k,c=1)
{ 
   if(!is.data.frame(data) || !is.numeric(c(year,month,day.beg,day.end,k)) || day.beg>day.end)
      stop("invalid input parameter(s) specification")
  
   time.int<-(0:floor(24/k))*k
   m<-length(time.int)
   period<-day.beg:day.end
   data.zhr<-as.data.frame(setNames(replicate(5,numeric(0)), c("sollong","nINT","n.tot","ZHR","st.err"))) 
   data(shw_list,envir=environment())
   shw_list<-get("shw_list",envir=environment())   
   V<-shw_list$V[shw_list$Shw==shw]

   if(is.null(r))
       r<-shw_list$r[shw_list$Shw==shw] 
        
  data.h<-sinh(data,shw, Ralpha, Delta)
  
  for(i in 0:(length(period)-1)) {
        data.day<-data.h[data.h$day.new==day.beg+i & data.h$month==month,]
        mid.time<-mid.time(data.day)   
        sollong=nINT=n.tot=ZHR=st.err=rep(NA,m-1) 
   
    for(j in 1:(m-1)){
        ind<-mid.time>=time.int[j] & mid.time<time.int[j+1]               
        if(any(ind)){             
           T<-sum((data.day$sine.h[ind]*data.day$Teff[ind])/(data.day$F[ind]*r^(6.50-data.day$lmg[ind])))
           nINT[j]<-length(data.day[ind,which(names(data.day)=="N")])
           n.tot[j]<-sum(data.day[ind,which(names(data.day)=="N")])
           ZHR[j]<-(n.tot[j]+c)/T
           st.err[j]<-ZHR[j]/sqrt(n.tot[j]+c)
       }
                                         
        sollong[j]<-solar.long(year,month,day.beg+i,mean(c(time.int[j],time.int[j+1])))
   }
    data.zhr<-rbind(data.zhr,data.frame(sollong,nINT,n.tot,ZHR,st.err)) 
  }

  names(data.zhr)[3]<-paste("n",shw,sep="")  
  density<-round((10.65*r-12.15)*data.zhr$ZHR/(3600*178700*r^(-1.82)*V)*10^9,1)
  dens.err<-round(density*data.zhr$st.err/data.zhr$ZHR,1)
  data.zhr$ZHR<-round(data.zhr$ZHR,1)
  data.zhr$st.err<-round(data.zhr$st.err,1)
  data.frame(day=rep(day.beg:day.end,each=m-1),month=rep(month,m-1),year=rep(year,m-1),start=rep(time.int[-m],length(period)), 
             stop=rep(time.int[-1],length(period)),data.zhr, density,dens.err)
}
