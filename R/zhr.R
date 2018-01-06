zhr<-function(data,date.start,date.end,shw,r=NULL,kmin=0.01,kmax=1,num,c.zhr=0.5,rdata=NULL)
{ 
   if(is.null(r)&& !is.data.frame(rdata))
      stop("Invalid input parameter specification: specify value of r or dataframe rdata with calculated population index values")
   
   if(!(is.null(r))&& (r<1.5 || r>4.5))
      stop("Invalid input parameter specification: check value of r")
      
   if(is.null(r)&& is.data.frame(rdata) && (!("sollong"%in%names(rdata))|| !("pop.index"%in%names(rdata))))
      stop("Error: rdata does not contain columns named sollong and pop.index")
      
   if(!is.numeric(c.zhr) || c.zhr<0 || c.zhr>1)
      stop("Invalid input parameter specification: check value of c.zhr")
   
   if(!(all(c("Teff","F","Lmg")%in%names(data))))
     stop("Error: data does not contain columns named Teff, F and Lmg")
      
   
   data(shw_list,envir=environment())
   shw_list<-get("shw_list",envir=environment())  
   V<-shw_list$V[shw_list$Shw==shw]
   rconst<-shw_list$r[shw_list$Shw==shw]
   
   year<-year(date.start)


   
   results<-as.data.frame(replicate(8,numeric(0)))
   names(results)<-c("sollong","date","nINT","nSHW",
                     "ZHR","st.error","density","dens.error")
                     
   zerolong<-sollong_date(0,year)
                     
   if(zerolong>=as.POSIXct(date.start,tz="UTC") && zerolong<=as.POSIXct(date.end,tz="UTC")){
    blocks<-c(opt.bin(data,date.start,round_date(zerolong-30,unit="minute"),shw,kmin,kmax,num),
              opt.bin(data,round_date(zerolong+30,unit="minute"),date.end,shw,kmin,kmax,num))
   }else{blocks<-opt.bin(data,date.start,date.end,shw,kmin,kmax,num)}
   
   
    for(j in 1:length(blocks)){
    sollong<-round(weighted.mean(blocks[[j]]$Sollong,blocks[[j]]$Teff*blocks[[j]]$sine.h/(blocks[[j]]$F*rconst^(6.50-blocks[[j]]$Lmg))),3)
    date<-sollong_date(sollong,year,date.start,date.end)
    
     if(is.null(r))                                                              
       r<-spline(rdata$sollong,rdata$pop.index,method="natural",xout=sollong)$y
    
   
    
    Ti<-blocks[[j]]$Teff*blocks[[j]]$sine.h/(blocks[[j]]$F*r^(6.50-blocks[[j]]$Lmg))      
    nSHW<-sum(blocks[[j]]$Number)
    nINT<-nrow(blocks[[j]])
    T<-sum(Ti)
    ZHR<-(nSHW+c.zhr)/T
    st.error<-sqrt(nSHW+c.zhr)/T
    density<-(10.65*r-12.15)*ZHR/(3600*178700*r^(-1.82)*V)*10^9
    dens.error<-density*st.error/ZHR
  
    results<-rbind(results,data.frame(sollong,date,nINT,nSHW,ZHR,st.error,density,
                               dens.error))
                 
      
       
   }
                                                       
  names(results)[4]<-paste("n",shw,sep="")  
  results[,5:8]=round(results[,5:8],1)
  
  results
}
