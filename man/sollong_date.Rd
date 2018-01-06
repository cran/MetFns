\name{sollong_date}
\alias{sollong_date}
\title{
Calculation of date corresponding to given value of solar longitude
}
\description{
Calculates calendar date and time corresponding to specified value of solar longitude (J2000.0) and specified year.
}
\usage{
sollong_date(solval,year,date1=NULL,date2=NULL)
}
\arguments{
\item{solval}{
 numeric vector of length one specifying solar longitude.
}
\item{year}{
 numeric vector of length one specifying year.
}
\item{date1}{
character vector of length one specifying start date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{date2}{
character vector of length one specifying end date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). 
}
}
\details{
The function \code{sollong_date} calculates date corresponding to the given value of solar longitude, for specified year between 1984 and 2030. It searches for a solution in a interval
bounded by \code{date1} and \code{date2}. If user does not specify \code{date1} and \code{date2}, they are, by default, equal to the 1st od January and the 31st of December, respectively.
}
\value{
\code{sollong_date} returns object of POSIXct class, calendar date in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\author{
Kristina Veljkovic
}
\note{Arguments \code{date1} and \code{date2} should belong to the same year. 
}
\seealso{
\code{\link{date_sollong}}
}
\examples{
## calculate date corresponding to 1 degree of solar longitude 
## search between dates 1 and 31 March 2017
sollong_date(1,year=2017,"2017-03-01","2017-03-31")
}

