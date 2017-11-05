filter.sol<-function(data, sol.low, sol.up)                
{
   if(!is.data.frame(data)) 
      stop("Invalid input parameter specification: check data")
      
   if(!is.numeric(c(sol.low,sol.up)) || any(c(sol.low,sol.up)<0) || any(c(sol.low,sol.up)>360)) 
      stop("Invalid input parameter(s) specification: check value(s) of sol.low/sol.up")
      
   if(!("Sollong"%in%names(data)))
     stop("Error: data does not contain column named Sollong")
  
  
   year<-year(midint(data[ceiling(nrow(data)/2),]))

   if(sol.low<sol.up)
     data[data$Sollong>=sol.low & data$Sollong<=sol.up,]
   else if(sol.low>sol.up && sol.low>=date_sollong(paste(as.character(year),"-01-01", sep="")) && 
           sol.up<=date_sollong(paste(as.character(year),"-12-31 23:59:59", sep=""))) 
     rbind(data[data$Sollong>=sol.low,],data[data$Sollong<=sol.up,])
   else
     stop("Invalid input parameter(s) specification: check value(s) of sol.low/sol.up")
}

