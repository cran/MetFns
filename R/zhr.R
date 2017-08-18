zhr<-function(data,date.start,date.end,shw,r=NULL,kmin=0.01,kmax=1,num,c.zhr=0.5,rdata=NULL)
{ 
   if((is.null(r)&& (!is.data.frame(rdata)|| !("sollong"%in%names(rdata))|| !("pop.index"%in%names(rdata)))))
      stop("invalid input parameter(s) specification: check r/rdata")
   
   data(shw_list,envir=environment())
   shw_list<-get("shw_list",envir=environment())  
   V<-shw_list$V[shw_list$Shw==shw]
   rconst<-shw_list$r[shw_list$Shw==shw]
   


   
   results<-as.data.frame(replicate(8,numeric(0)))
   names(results)<-c("sollong","date","nINT","nSHW",
                     "ZHR","st.err","density","dens.err")

   blocks<-opt.bin(data,date.start,date.end,shw,kmin,kmax,num)
   
   
    for(j in 1:length(blocks)){
    sollong<-round(weighted.mean(blocks[[j]]$Sollong,blocks[[j]]$Teff*blocks[[j]]$sine.h/(blocks[[j]]$F*rconst^(6.50-blocks[[j]]$Lmg))),3)
    date<-sollong_date(sollong,date.start,date.end)
    
     if(is.null(r))                                                              
       r<-spline(rdata$sollong,rdata$pop.index,method="natural",xout=sollong)$y
    
   
    
    Ti<-blocks[[j]]$Teff*blocks[[j]]$sine.h/(blocks[[j]]$F*r^(6.50-blocks[[j]]$Lmg))      
    nSHW<-sum(blocks[[j]]$Number)
    nINT<-nrow(blocks[[j]])
    T<-sum(Ti)
    ZHR<-(nSHW+c.zhr)/T
    st.err<-sqrt(nSHW+c.zhr)/T
    density<-(10.65*r-12.15)*ZHR/(3600*178700*r^(-1.82)*V)*10^9
    dens.err<-density*st.err/ZHR
  
    results<-rbind(results,data.frame(sollong,date,nINT,nSHW,ZHR,st.err,density,
                               dens.err))
                 
      
       
   }
                                                       
  names(results)[4]<-paste("n",shw,sep="")  
  results[,5:8]=round(results[,5:8],1)
  
  results
}
