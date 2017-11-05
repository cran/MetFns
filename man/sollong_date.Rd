\name{sollong_date}
\alias{sollong_date}
\title{
Calculation of date corresponding to given value of solar longitude
}
\description{
Calculates calendar date and time corresponding to specified value of solar longitude (J2000.0).
}
\usage{
sollong_date(solval,date1,date2)
}
\arguments{
\item{solval}{
 numeric vector specifying solar longitude.
}
\item{date1}{
character vector specifying start date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{date2}{
character vector specifying end date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). 
}
}
\details{
The function \code{sollong_date} calculates date corresponding to the given value of solar longitude. It searches for a solution in a interval
bounded by \code{date1} and \code{date2}. 
}
\value{
\code{sollong_date} returns object of POSIXct class, calendar date in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\author{
Kristina Veljkovic
}
\note{Arguments \code{date1} and \code{date2} should belong to the same year. 

Date corresponding to the zero longitude is returned only for years between 1984. and 2030. Also, calculation of dates corresponding to the specified value \code{solval} for \code{date1} before March equinox (with respect to J2000.0) and \code{date2} after March equinox (with respect to J2000.0), is possible only for years 1984-2030. Otherwise, user will be asked to change values of \code{date1} or \code{date2}.
}
\seealso{
\code{\link{date_sollong}}
}
\examples{
## calculate date corresponding to 1 degree of solar longitude 
## search between dates 1 and 31 March 2017
sollong_date(1,"2017-03-01","2017-03-31")
}

