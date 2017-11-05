filter<-function(data,date.start=NULL,date.end=NULL,shw=NULL,lat.low=-90,lat.up=90,long.low=-180,long.up=180, 
fname=NULL,lname=NULL,site=NULL,country=NULL,mag.low=1,mag.up=8,P.low=0,P.up=90,sol.low=NULL,sol.up=NULL,
h.low=0,h.up=90,r=NULL,C=NULL)
{
   data.select<-data

   if(!is.null(shw))
      data.select<-filter.shw(data.select, shw)   
   
   if(!is.null(date.start) && !is.null(date.end))
      data.select<-filter.date(data.select,date.start,date.end)
 
   if(!is.null(fname) && !is.null(lname)) 
      data.select<-filter.obsname(data.select,fname,lname)
 
   if(lat.low!=-90 || lat.up!=90 || long.low!=-180 || long.up!=180)
      data.select<-filter.gc(data.select,long.low,long.up,lat.low,lat.up)
  
   if(!is.null(site)) 
      data.select<-filter.site(data.select,site)
 
   if(!is.null(country)) 
      data.select<-filter.country(data.select,country)
 
   if(mag.low!=1 || mag.up!=8) 
      data.select<-filter.mag(data.select,mag.low,mag.up)

   if(P.low!=0 || P.up!=90) 
      data.select<-filter.P(data.select,P.low,P.up)
 
   if(!is.null(sol.low) && !is.null(sol.up)) 
      data.select<-filter.sol(data.select,sol.low,sol.up)

   if(!is.null(shw) && (h.low!=0 || h.up!=90)) 
      data.select<-filter.h(data.select,shw,h.low,h.up)
   
   if(!is.null(r) && !is.null(C))
      data.select<-filter.totcor(data.select,shw,r,C)

   data.select
}
