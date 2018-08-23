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
numeric vector specifying number of decimals of calculated solar longitude (between 2 and 5 decimals). By default, it is equal to 4.
}
}
\details{
Solar longitude is calculated applying VSOP87 theory, using coefficients from data \code{EarthVSOP87B}.
}
\value{
\code{sollong} returns solar longitude, numeric vector of length one.
}
\references{
Meeus, J. (1998). Astronomical algorithms (Second Edition).  Willmann-Bell, Inc., Richmond, Virginia, p. 219.

\url{http://neoprogrammics.com/vsop87}

}
\author{
Kristina Veljkovic
}
\note{
User can specify date without time (i.e. date at midnight) or date with hours and minutes (without seconds).
}
\seealso{
\code{\link{solar.long},\link{sollong_date}}
}
\examples{
## calculate solar longitude corresponding to March 7, 2016, at 02:41 UTC
date_sollong(date="2016-03-07 02:41")
}

