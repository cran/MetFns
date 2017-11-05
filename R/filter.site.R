filter.site<-function(data,site)
{
  if(!is.data.frame(data))
     stop("Invalid input parameter specification: check data")
  
  if(!is.character(site) || !grepl("^[a-zA-Z]+$",gsub(" ", "", site)))
     stop("Invalid input parameter specification: check site format")
     
  if(!("City"%in%names(data)))
     stop("Error: data does not contain column named City")
  
  if(!any(grepl(site,as.character(data$City))))
     stop("No data for selected site")
  
  
  data[substr(as.character(data$City),start=1,stop=nchar(site))==site,]
}
