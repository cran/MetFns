\name{date_sollong}
\alias{date_sollong}
\title{
Calculation of solar longitude
}
\description{
Calculates solar longitude with respect to the equinox of 2000.0 for given date.
}
\usage{
date_sollong(date,prec=4)
}
\arguments{
\item{date}{
character vector or factor of length one specifying date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{prec}{
numeric vector specifying number of decimals of calculated solar longitude (between 0 and 11 decimals). By default, it is equal to 4.
}
}
\value{
\code{sollong} returns solar longitude, numeric vector of length one.
}
\references{
Steyaert C. (1991). Calculating the Solar Longitude 2000.0, \emph{WGN, Journal of the IMO}, 19:2, 31-34.
}
\author{
Kristina Veljkovic
}
\note{
Hour, seconds and minutes does not have to be provided in function call.
}
\seealso{
\code{\link{solar.long},\link{sollong_date}}
}
\examples{
## calculate solar longitude corresponding to March 7, 2016, at 02:41 UTC, rounded to 3 decimals
date_sollong(date="2016-03-07 02:41",prec=3)
}

