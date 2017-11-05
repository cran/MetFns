filter.obsname<-function(data,fname,lname)
{
   if(!is.data.frame(data))
      stop("Invalid input parameter specification: check data")
      
   if(!is.character(c(fname,lname)) || !grepl("^[a-zA-Z]+$",fname) || !grepl("^[a-zA-Z]+$",lname))
      stop("Invalid input parameter(s) specification: check fname/lname format")
      
   if(!(all(c("First.Name","Last.Name")%in%names(data))))
     stop("Error: data does not contain columns named First.Name and Last.Name")
   
   if(!(fname%in%data$First.Name) || !(lname%in%data$Last.Name))
     stop("No data for selected first and last name")

   
   data[as.character(data$First.Name)==fname & as.character(data$Last.Name)==lname,]
   
}
