mid.time<-function(data)
{
  if(!is.data.frame(data)) 
     stop("invalid input parameter specification")
    
  time<-cbind(dec.time(data$start),dec.time(data$stop))
  midtime<-apply(time,1,mean)
  ind1<-time[,1]>time[,2]&midtime>=12
  ind2<-time[,1]>time[,2]&midtime<12

  midtime[ind1]<-midtime[ind1]-12
  midtime[ind2]<-midtime[ind2]+12
  midtime
}
