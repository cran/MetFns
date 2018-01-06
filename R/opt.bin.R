opt.bin<-function(data,date.start,date.end,shw,kmin=0.01,kmax=1,num)
{
   if(!is.data.frame(data))
      stop("Invalid input parameter specification: check data")
      
   if(!is.numeric(c(kmin,kmax,num)) || kmin<0.001 || kmax>5 || num<1)
      stop("Invalid input parameter(s) specification: check value(s) of kmin/kmax/num")
      
   if(!(all(c("Start.Date","End.Date","Number")%in%names(data))))
     stop("Error: data does not contain columns named Start.Date, End.Date and Number")


   sol1<-date_sollong(date.start)
   sol2<-date_sollong(date.end)

   data.shw<-filter(data,shw=shw,sol.low=sol1, sol.up=sol2)
   if(!("sine.h"%in%names(data.shw))) data.shw<-sinh(data.shw,shw)
   obs.len<-solar.long(data.shw$End.Date)-solar.long(data.shw$Start.Date)
   datashw<-cbind(data.shw,obs.len)
   o<-order(datashw$Sollong)
   datashw<-datashw[o,]
   datashw2<-datashw[datashw$obs.len<=kmax,]


   indlist<-list()
   m<-1

   p1<-datashw2$Sollong[1]
   i<-1
   n<-nrow(datashw2)
   flag<-FALSE
   while(i<=n){
    ind<-datashw2$Sollong>=p1 & datashw2$Sollong<=min(c(sol2,p1+kmax))& c(rep(FALSE,i-1),rep(TRUE,n-i+1))
    v<-cumsum(datashw2$Number[ind])>=num
    j<-which(ind)
    if(any(v)){
      for(k in min(which(v)):max(which(v))){
       bin<-datashw2$Sollong[i+k-1]-p1
       beg<-1
       end<-k
       ind2<-datashw2$obs.len[i:(i+k-1)]<=bin
       while(sum(datashw2$Number[i:(i+k-1)][ind2])>=num && bin>=kmin){
        if(min(which(ind2))==beg && max(which(ind2))==end)
        {j<-i-1+which(ind2)
         flag<-TRUE
         break}
        beg<-min(which(ind2))
        end<-max(which(ind2))
        bin<-datashw2$Sollong[end+i-1]-datashw2$Sollong[beg+i-1]
        ind2<-datashw2$obs.len[i:(i+k-1)]<=bin
      }
       if(any(flag)){break}
    }
   }

   indlist[[m]]<-datashw2[j,]
   m<-m+1
   i<-max(j)+1
   p1<-datashw2$Sollong[i]
   flag<-FALSE


   }
   indlist
}