filter.totcor<-function(data,shw,r,C=5)
{
 if(!is.numeric(c(r,C)) || (C<1) || (r<1.5) || (r>4.5)) 
      stop("invalid input parameter(s) specification: check r/C")

 elev.data<-sinh(data,shw)
 m<-ncol(elev.data)
 elev.data[elev.data$F*r^(6.5-elev.data$Lmg)/elev.data$sine.h<=C,-m]
}
