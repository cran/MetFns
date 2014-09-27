day.mid<-function(data)
{   
  day.new<-data$day
  mid.int<-mid.time(data)
  ind<-data$start>data$stop & mid.int<12
  day.new[ind]<-data$day[ind]+1
  
  cbind(mid.int,day.new)
}
