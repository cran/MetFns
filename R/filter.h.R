filter.h<-function(data,shw,h.low=0,h.up=90)
{
 if(!is.numeric(c(h.low,h.up)) || (h.low<0 || h.up>90) || h.low>h.up) 
      stop("Invalid input parameter(s) specification: check value(s) of h.low/h.up")

 elev.data<-sinh(data,shw)
 h<-asin(elev.data$sine.h)*180/pi
 elev.data[h>=h.low & h<=h.up,]
}
