pop.index2<-function(data,date.start,date.end,shw,kmin=0.01,kmax=1,num,gamma=1)
{ 
  if(!is.numeric(gamma) || gamma<1 || gamma>2)
    stop("Invalid input parameter specification: check value of gamma")
  
  if(!(all(c("F","Lmg","Mag.N6","Mag.N5","Mag.N4","Mag.N3","Mag.N2","Mag.N1",
              "Mag.0","Mag.1","Mag.2","Mag.3","Mag.4","Mag.5","Mag.6","Mag.7")%in%names(data))))
     stop("Error: data does not contain columns named F, Lmg, Mag.N6, Mag.N5, 
           Mag.N4, Mag.N3, Mag.N2, Mag.N1, Mag.0, Mag.1,Mag.2, Mag.3, Mag.4, Mag.5, Mag.6 and Mag.7")

   
   
   data(popind,envir=environment())
   popind<-get("popind",envir=environment())
   
   data(popind.err,envir=environment())
   popind.err<-get("popind.err",envir=environment())
   
   data(shw_list,envir=environment())
   shw_list<-get("shw_list",envir=environment())
   r<-shw_list$r[shw_list$Shw==shw]


   year<-year(date.start)
   
   results<-as.data.frame(replicate(6,numeric(0)))
   names(results)<-c("sollong","date","nINT","nSHW","pop.index","r.error")
   
   mag.val<--6:7
   
   x<-seq(1.5,3.5,by=0.1)
   y<-log(c(10,15,22,33,49,73,109,163,244,366,549,823,1234,1851,2776,4164,6246,9369))
   z<-matrix(popind.err$r.err,nrow=21,ncol=18,byrow=T)


   zerolong<-sollong_date(0,year)
                     
   if(zerolong>=as.POSIXct(date.start,tz="UTC") && zerolong<=as.POSIXct(date.end,tz="UTC")){
    blocks<-c(opt.bin(data,date.start,round_date(zerolong-30,unit="minute"),shw,kmin,kmax,num),
              opt.bin(data,round_date(zerolong+30,unit="minute"),date.end,shw,kmin,kmax,num))
   }else{blocks<-opt.bin(data,date.start,date.end,shw,kmin,kmax,num)}
   
    
    for(j in 1:length(blocks)){
             
    sollong<-round(weighted.mean(blocks[[j]]$Sollong,blocks[[j]]$Number*(blocks[[j]]$sine.h)^gamma/(blocks[[j]]$F*r^(6.50-blocks[[j]]$Lmg))),3)
    date<-sollong_date(sollong,year,date.start,date.end)
    
    

    coefdm<-blocks[[j]]$Number*blocks[[j]]$Lmg- 
     apply(as.matrix(blocks[[j]][,which(names(blocks[[j]])=="Mag.N6"):which(names(blocks[[j]])=="Mag.7")])%*%diag(mag.val),1,sum)

    nSHW<-sum(blocks[[j]]$Number)
    mean.deltam<-sum(coefdm)/nSHW
    
    if(nSHW<10) {
      pop.index<-r.error<-NA
    } else{
      pop.index<-spline(popind$avdeltam,popind$r,method="natural",xout=mean.deltam)$y
      if(nSHW>9369){
        r.error<-predict(model<-rerr.reg(popind.err),data.frame(r=rep(pop.index,2),logn=rep(log(nSHW),2)))[1]
      } else{
        r.error<-round(interp2(x,y,t(z),pop.index,log(nSHW)),6)}
    }
   
                
   nINT<-nrow(blocks[[j]])
   
   results<-rbind(results,data.frame(sollong,date,nINT,nSHW,pop.index,r.error))
                     
       
   }
   
  names(results)[4]<-paste("n",shw,sep="") 
  results$pop.index<-round(results$pop.index,2)
  results$r.error<-round(results$r.error,2)
  
 
  results
 
}


