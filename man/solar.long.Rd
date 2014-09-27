\name{solar.long}
\alias{solar.long}
\title{
Calculation of solar longitude
}
\description{
Calculates solar longitude with respect to the equinox of 2000.0 for given year, month, day and time.
}
\usage{
solar.long(year, month, day, time)
}
\arguments{
  \item{year}{
 numeric vector of length 4 specifying year.
}
   \item{month}{
numeric vector specifying month of the year.
}
   \item{day}{
numeric vector specifying day.
}
   \item{time}{
numeric vector specifying time in hours.
}
}
\value{
\code{solar.long} returns numeric vector with a value between 0 and 359.999.
}
\references{
Steyaert C. (1991). Calculating the Solar Longitude 2000.0, \emph{WGN, Journal of the IMO}, 19:2, 31-34.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{filter.sol}}
}
\examples{
## calculate solar longitude for June 22, 2006, at 4h UT.
require(astroFns)
solar.long(year=2006,month=6,day=22,time=4)
}

