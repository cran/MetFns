filter.obsname<-function(data,fname,lname)
{
   if(!is.data.frame(data) || !is.character(c(fname,lname)) || !grepl("^[a-zA-Z]+$",fname) || !grepl("^[a-zA-Z]+$",lname))
      stop("invalid input parameter(s) specification: check data/fname/lname")
   
   
   data[as.character(data$First.Name)==fname & as.character(data$Last.Name)==lname,]
   
}
