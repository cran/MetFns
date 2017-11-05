\name{solar.long}
\alias{solar.long}
\title{
Vectorized calculation of solar longitude
}
\description{
Calculates solar longitudes with respect to the equinox of 2000.0 for given dates.
}
\usage{
solar.long(date,prec=4)
}
\arguments{
 \item{date}{
character vector or factor specifying dates, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{prec}{
numeric vector specifying number of decimals of calculated solar longitude (between 2 and 5 decimals). By default it is equal to 4.
}
}
\details{
The function \code{solar.long} represents vectorized version of function \code{sollong}. It calculates solar longitudes corresponding to multiple dates.}
\value{
\code{solar.long} returns solar longitude(s), numeric vector with specified number of decimal places.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{date_sollong},\link{filter.sol}}
}
\examples{
## calculate solar longitudes corresponding to the middle of time intervals 
## of 2015 Orionids rate data
## first select Orionids from 2015 rate data
rateOri<-filter.shw(rate2015,shw="ORI")

## calculate middle of observing time intervals of 2015 Orionids rate data
midintOri<-midint(rateOri)

## calculate solar longitudes corresponding to the middle of time intervals
solar.long(midintOri)

}

