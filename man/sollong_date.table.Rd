\name{sol_date.table}
\alias{sol_date.table}
\title{
Vectorized calculation of date corresponding to given value of solar longitude
}
\description{
Calculates table of dates corresponding to the given values of solar longitude and year.
}
\usage{
sol_date.table(solval,year)
}
\arguments{
\item{solval}{
 numeric vector specifying solar longitude(s).
}
\item{year}{
 numeric vector specifying year(s).
}
}
\details{
The function \code{sol_date.table} represents vectorized version of function \code{sollong_date}. It calculates table of dates corresponding to multiple values of either solar longitude or year.}
\value{
\code{sol_date.table} returns dataframe. Each column is object of POSIXct class, calendar dates in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). Column names are solar longitudes and row names are years.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{sollong_date}}
}
\examples{
## calculate dates corresponding to the solar longitudes 347-350, year 2016 
sol_date.table(347:350,year=2016)

## calculate dates corresonding to the solar longitude 347, years 2015-2017
sol_date.table(347,year=2015:2017)
}

