date_sollong<-function(date,prec=4)
{   
   date.new<-tryCatch(as.POSIXct(date,tz="UTC"),error=function(e){return(NA)}) 
   
   if(is.na(date.new))
     stop("Invalid input parameter specification: check date format")
     
   if(!(prec%in%2:5))
     stop("Invalid input parameter specification: check value of precision")
     
   options(digits=15)
     
   year<-year(date.new)
   month<-month(date.new)
   day<-day(date.new)
   time<-hour(date.new)+minute(date.new)/60+second(date.new)/3600

   data(EarthVSOP87B,envir=environment())
   vsop87b<-get("EarthVSOP87B",envir=environment())
   terms<-split(vsop87b,vsop87b$Subseries)
   
   t<-(ymd2jd(year,month,day)+(time+deltaT(year,month)/3600)/24-2451545)/365250
   
   L0<-sum((terms$L0)$A*cos((terms$L0)$B+t*(terms$L0)$C))
   L1<-sum((terms$L1)$A*cos((terms$L1)$B+t*(terms$L1)$C))*t
   L2<-sum((terms$L2)$A*cos((terms$L2)$B+t*(terms$L2)$C))*t^2
   L3<-sum((terms$L3)$A*cos((terms$L3)$B+t*(terms$L3)$C))*t^3
   L4<-sum((terms$L4)$A*cos((terms$L4)$B+t*(terms$L4)$C))*t^4
   L5<-sum((terms$L5)$A*cos((terms$L5)$B+t*(terms$L5)$C))*t^5
 
   B0<-sum((terms$B0)$A*cos((terms$B0)$B+t*(terms$B0)$C))
   B1<-sum((terms$B1)$A*cos((terms$B1)$B+t*(terms$B1)$C))*t
   B2<-sum((terms$B2)$A*cos((terms$B2)$B+t*(terms$B2)$C))*t^2
   B3<-sum((terms$B3)$A*cos((terms$B3)$B+t*(terms$B3)$C))*t^3
   B4<-sum((terms$B4)$A*cos((terms$B4)$B+t*(terms$B4)$C))*t^4
   B5<-sum((terms$B5)$A*cos((terms$B5)$B+t*(terms$B5)$C))*t^5
   
   B <- B0+B1+B2+B3+B4+B5
   L <- L0+L1+L2+L3+L4+L5
   Ldeg<- L * 180/pi

   Ldegp<- Ldeg-(1.397 * 10*t) - (0.00031 * 100*(t^2))
   deltaL<- (-0.09033 + (0.03916 * (cos(Ldegp *pi/180) + sin(Ldegp *pi/180))) * tan(B))/3600
        
   Ldeg<- Ldeg+deltaL  

   round((Ldeg+ 180)%%360,prec)    
  
}
