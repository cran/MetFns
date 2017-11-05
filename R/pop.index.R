pop.index<-function(data,date.start,date.end,shw, mag.range=-6:7,k)
{ 
   if(!is.data.frame(data))
      stop("Invalid input parameter specification: check data")  
      
   if(!is.numeric(mag.range) || !all(mag.range%in%(-6:7)))
      stop("Invalid input parameter specification: check values of mag.range")
      
   if(!is.numeric(k) || k<0.001 || k>5)
      stop("Invalid input parameter specification: check value of k")   
      
   if(!(all(c("Start.Date","End.Date","F","Lmg","Mag.N6","Mag.N5","Mag.N4","Mag.N3","Mag.N2","Mag.N1",
              "Mag.0","Mag.1","Mag.2","Mag.3","Mag.4","Mag.5","Mag.6","Mag.7","Number")%in%names(data))))
     stop("Error: data does not contain columns named Start.Date, End.Date, F, Lmg, Mag.N6, Mag.N5, 
           Mag.N4, Mag.N3, Mag.N2, Mag.N1, Mag.0, Mag.1,Mag.2, Mag.3, Mag.4, Mag.5, Mag.6, Mag.7 and Number")

     
   sol1<-date_sollong(date.start)
   sol2<-date_sollong(date.end)
   
   data(shw_list,envir=environment())
   shw_list<-get("shw_list",envir=environment())
   r<-shw_list$r[shw_list$Shw==shw]
   
   data.shw<-filter(data,shw=shw,sol.low=sol1, sol.up=sol2)
   obs.len<-solar.long(data.shw$End.Date)-solar.long(data.shw$Start.Date)
   if(!("sine.h"%in%names(data.shw))) data.shw<-sinh(data.shw,shw)
   datashw<-cbind(data.shw,obs.len)
   o<-order(datashw$Sollong)
   datashw<-datashw[o,]
   datashw2<-datashw[datashw$obs.len<=k,]
   
   results<-as.data.frame(replicate(7,numeric(0)))
   names(results)=c("sollong","date","mag","nINT","nSHW","pop.index","r.error")

   deltam<-(-4:74)/10
   p<-c(0.00046,0.00074,0.0011,0.0016,0.0023,0.0033,0.0046,0.0063,0.0081,0.0100,0.0122,
   0.015,0.018,0.020,0.023,0.026,0.030,0.034,0.039,0.044,0.049,0.056,0.063,0.071,0.079,
   0.088,0.10,0.11,0.13,0.14,0.16,0.18,0.20,0.22,0.24,0.26,0.29,0.32,0.35,0.38,0.40,0.43,
   0.46,0.49,0.52,0.53,0.55,0.59,0.64,0.66,0.67,0.69,0.71,0.73,0.74,0.76,0.77,0.78,0.79,
   0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.90,0.91,0.92,0.93,0.935,0.94,0.95,
   0.96,0.97,0.98)
   mag.val<--6:7 

   p1<-datashw2$Sollong[1]
   i<-1
   n<-nrow(datashw2)
   while(i<=n){
    ind<-datashw2$Sollong>=p1 & datashw2$Sollong<=min(c(sol2,p1+k))
    
    sollong<-round(weighted.mean(datashw2$Sollong[ind],datashw2$Number[ind]*datashw2$sine.h[ind]/(datashw2$F[ind]*r^(6.50-datashw2$Lmg[ind]))),3)
    date<-sollong_date(sollong,date.start,date.end)
    
    deltam.obs<-round(rep(datashw2$Lmg[ind],each=14)-mag.val,1)
    coef<-p[match(deltam.obs,deltam)]
    coef[deltam.obs>7.4]<-1
    coef[deltam.obs<(-0.4)]<-0.0001
                 
    mag.distrib<-datashw2[ind,which(names(datashw2)=="Mag.N6"):which(names(datashw2)=="Mag.7")]/
                               matrix(coef,ncol=14,byrow=TRUE)
                 
    counts<-t(apply(mag.distrib,2,sum))
    cum.freq<-cumsum(counts)
    
    indm<-mag.val %in% mag.range & (counts>=3) & mag.val<=5
    m<-mag.val[indm]
    mag<-ifelse(length(m)>0,paste(m[1],":",m[length(m)],sep=""),"-6:7")
    if(length(m)>4){
       cum.freq<-cum.freq[indm]
       mag0.ind<-match(0,m)
       y<-log(cum.freq/cum.freq[mag0.ind])
       a_est<-sum(m*y)/sum(m^2)
       pop.index<-exp(a_est)
       var.a<-sum((y-a_est*m)^2)/((length(m)-2)*sum(m^2))
       r.error<-pop.index*sqrt(exp(var.a)*(exp(var.a)-1)) 
    } else{pop.index<-r.error<-NA}
                  
    nINT<-nrow(datashw2[ind,])
    nSHW<-sum(datashw2$Number[ind])
    results<-rbind(results,data.frame(sollong,date,mag,nINT,nSHW,pop.index,r.error))
         
 
    i<-max(which(ind))+1
    p1<-datashw2$Sollong[i]     
  }
   
   
  names(results)[5]<-paste("n",shw,sep="") 
  results$pop.index<-round(results$pop.index,2)
  results$r.error<-round(results$r.error,2)
 
 
  
  results
 
}


