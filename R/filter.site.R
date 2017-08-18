filter.site<-function(data,site)
{
  if(!is.data.frame(data) || !is.character(site) || !grepl("^[a-zA-Z]+$",gsub(" ", "", site)))
     stop("invalid input parameter(s) specification: check data/site")
  
  data[substr(as.character(data$City),start=1,stop=nchar(site))==site,]
}
