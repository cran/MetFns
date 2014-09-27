filter.date<-function(data,year,month, day.beg,day.end=day.beg)
{ 
  if(!is.data.frame(data) || !is.numeric(c(year,month,day.beg,day.end)) || year<1984
     || (month<1) || (month>12) || (any(c(day.beg,day.end)<1)) || any(c(day.beg,day.end)>31))
     stop("invalid input parameter(s) specification")
  
  day<-day.mid(data)[,2]
  data[(data$year==year%%100) & (data$month==month) & (day>=day.beg & day<=day.end),]
}
