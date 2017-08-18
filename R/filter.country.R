filter.country<-function(data,country)
{
  if(!is.data.frame(data) || !is.character(country) || !grepl("^[a-zA-Z]+$",gsub(" ", "", country)))
     stop("invalid input parameter(s) specification: check data/country")
 
  data[as.character(data$Country)==country,]
}
