\name{sollong_date}
\alias{sollong_date}
\title{
Calculation of date corresponding to given value of solar longitude
}
\description{
Calculates calendar date and time corresponding to specified value of solar longitude.
}
\usage{
sollong_date(solval,date1,date2,unit="minute")
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
\item{unit}{
character vector specifying time unit to be result rounded to. Possible values are "minute"(default) and "second".
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
\note{Arguments \code{date1} and \code{date2} should belong to the same year.}
\seealso{
\code{\link{date_sollong}}
}
\examples{
## calculate date corresponding to 1 degree of solar longitude 
## search between dates 1 and 31 March 2017
sollong_date(1,"2017-03-01","2017-03-31")
}

