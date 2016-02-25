\name{sollong_date}
\alias{sollong_date}
\title{
Calculation of date and time corresponding to given solar longitude
}
\description{
Calculates calendar date and time corresponding to specified value of solar longitude and year within given month (s), day (s) and time period.
}
\usage{
sollong_date(year,value,month.beg=1,month.end=12,day.beg=1,day.end=31,
time.beg=0,time.end=2359)
}
\arguments{
  \item{year}{
 numeric vector of length 4 specifying year.
}
\item{value}{
 numeric vector specifying solar longitude.
}
   \item{month.beg}{
numeric vector specifying the beginning month. By default, \code{month.beg} is set to be equal to 1.
}
  \item{month.end}{
numeric vector specifying the ending month. By default, \code{month.end} is set to be equal to 12.
}
  \item{day.beg}{
numeric vector specifying the beginning day. By default, \code{day.beg} is set to be equal to 1.
}
  \item{day.end}{
numeric vector specifying the ending day. By default, \code{day.end} is set to be equal to 31.
}
  \item{time.beg}{
numeric vector (0-2359) specifying lower boundary of time in hours and minutes, corresponding to \code{day.beg}. By default, \code{time.beg} is set to be equal to 0.
}
  \item{time.end}{
numeric vector(0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}
}
\value{
\code{solar.long} returns object of POSIXct class - calendar date plus time in hours, minutes and seconds, in UTC.
}

\author{
Kristina Veljkovic
}
\seealso{
\code{\link{solar.long}}
}
\examples{
## calculate date and time corresponding to 0.9 solar longitude for year 2000, 
##search between 1 and 31 March
sollong_date(year=2000,value=0.9,month.beg=3,month.end=3,day.beg=1,day.end=31)
}

