filter.country<-function(data,country)
{
  if(!is.data.frame(data))
     stop("Invalid input parameter specification: check data")
     
  if(!is.character(country) || !grepl("^[a-zA-Z]+$",gsub(" ", "", country)))
     stop("Invalid input parameter specification: check country format")
  
  if(!("Country"%in%names(data)))
     stop("Error: data does not contain column named Country")
     
  if(!(country%in%as.character(data$Country)))
     stop("No data for selected country")
 
  data[as.character(data$Country)==country,]
}
