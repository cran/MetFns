filter.shw<-function(data, shw)
{
  if(!is.data.frame(data))
      stop("invalid input parameter specification: check data")
      
  if(!is.character(shw) || nchar(shw)!=3 || !grepl("^[A-Z]+$",shw) )
      stop("invalid input parameter specification: check shw format")
      
  if(!("Shower"%in%names(data)))
     stop("Error: data does not contain column named Shower")
  
  if(!(shw%in%as.character(data$Shower)))
      stop("No data for selected shower code")
      
  data[as.character(data$Shower)==shw,]
}
