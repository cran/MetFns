filter.totcor<-function(data,shw,r,C)
{
 if(!is.numeric(r) || r<1.5 || r>4.5) 
      stop("Invalid input parameter specification: check value of r")
      
 if(!is.numeric(C) || C<1) 
      stop("Invalid input parameter specification: check value of C")
 
 if(!(all(c("F","Lmg")%in%names(data))))
     stop("Error: data does not contain columns named F and Lmg")
      

 elev.data<-sinh(data,shw)
 m<-ncol(elev.data)
 elev.data[elev.data$F*r^(6.5-elev.data$Lmg)/elev.data$sine.h<=C,-m]
}
