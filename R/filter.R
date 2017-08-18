filter<-function(data,date.start=NULL,date.end=NULL,shw=NULL,lat.low=NULL,lat.up=NULL,long.low=NULL,long.up=NULL, 
fname=NULL,lname=NULL,site=NULL,country=NULL,mag.low=NULL,mag.up=NULL,P.low=NULL,P.up=NULL,sol.low=NULL,sol.up=NULL,
h.low=NULL,h.up=NULL,r=NULL,C=NULL)
{
   data.select<-data

   if(!is.null(shw))
      data.select<-filter.shw(data.select, shw)   
   
   if(!is.null(date.start) && !is.null(date.end))
      data.select<-filter.date(data.select,date.start,date.end)
 
   if(!is.null(fname) && !is.null(lname)) 
      data.select<-filter.obsname(data.select,fname,lname)
 
   if(!is.null(lat.low) && !is.null(lat.up) && !is.null(long.low) && !is.null(long.up))
      data.select<-filter.gc(data.select,long.low,long.up,lat.low,lat.up)
  
   if(!is.null(site)) 
      data.select<-filter.site(data.select,site)
 
   if(!is.null(country)) 
      data.select<-filter.country(data.select,country)
 
   if(!is.null(mag.low) && !is.null(mag.up)) 
      data.select<-filter.mag(data.select,mag.low,mag.up)

   if(!is.null(P.low) && !is.null(P.up)) 
      data.select<-filter.P(data.select,P.low,P.up)
 
   if(!is.null(sol.low) && !is.null(sol.up)) 
      data.select<-filter.sol(data.select,sol.low,sol.up)

   if(!is.null(h.low) && !is.null(h.up)) 
      data.select<-filter.h(data.select,shw,h.low,h.up)
   
   if(!is.null(r) && !is.null(C))
      data.select<-filter.totcor(data.select,shw,r,C)

   data.select
}
