filter.sol<-function(data, sol.low, sol.up)                
{
   if(!is.data.frame(data) || !is.numeric(c(sol.low,sol.up)) || any(c(sol.low,sol.up)<0) || any(c(sol.low,sol.up)>360)) 
      stop("invalid input parameter(s) specification: check data/sol.low/sol.up ")

   year<-year(midint(data[100,]))

   if(sol.low<sol.up)
     data[data$Sollong>=sol.low & data$Sollong<=sol.up,]
   else if(sol.low>sol.up && sol.low>=date_sollong(paste(as.character(year),"-01-01", sep="")) && 
           sol.up<=date_sollong(paste(as.character(year),"-12-31 23:59:59", sep=""))) 
     rbind(data[data$Sollong>=sol.low,],data[data$Sollong<=sol.up,])
   else
     stop("invalid input parameter(s) specification: check value(s) of sol.low/sol.up")
}

