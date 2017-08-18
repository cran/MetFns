filter.shw<-function(data, shw)
{
  if(!is.data.frame(data) || !is.character(shw) || nchar(shw)!=3 || !grepl("^[A-Z]+$",shw) )
      stop("invalid input parameter(s) specification: check data/shw ")
      
  data[as.character(data$Shower)==shw,]
}
