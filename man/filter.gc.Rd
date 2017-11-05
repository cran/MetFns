\name{filter.gc}
\alias{filter.gc}
\title{
Selection of visual meteor data by geographical coordinates
}
\description{
Selects data for a given visual meteor dataset and specified geographical coordinates of the observing site 
or interval of geographical coordinates.
}
\usage{
filter.gc(data,lat.low =-90,lat.up = 90,long.low =-180,long.up = 180)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{lat.low}{
numeric vector taking a value between -90 (default) and 90, specifying lower boundary of latitude in degrees.
}
  \item{lat.up}{
numeric vector taking a value between -90 and 90 (default), specifying upper boundary of latitude in degrees.
}
  \item{long.low}{
numeric vector taking a value between -180 (default) and 180, specifying lower boundary of longitude in degrees.
}
  \item{long.up}{
numeric vector taking a value between -180 and 180 (default), specifying upper boundary of longitude in degrees.
}
}
\details{
If values of arguments \code{lat.low} and \code{lat.up}, as well as \code{long.low} and \code{long.up}, are the same,
\code{filter.gc} selects data for  particular observing site.

\code{filter.gc} enables one to select data only by latitude or longitude, with geographical coordinates
being between given boundaries, less, greater or equal to a boundary.
}
\value{
\code{filter.gc} returns data frame with the same number of columns as the argument \code{data},
containing observations corresponding to geographical coordinates with latitude between \code{lat.low} and \code{lat.up} and 
longitude between \code{long.low} and \code{long.up}.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "Latitude" and "Longitude".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select 2015 rate and magnitude data for observing site with latitude 44.1583N 
## and longitude 19.6869E
filter.gc(rate2015,lat.low=44.1583,lat.up=44.1583,long.low=19.6869,long.up=19.6869)
filter.gc(magn2015,lat.low=44.1583,lat.up=44.1583,long.low=19.6869,long.up=19.6869)

## select 2015 rate and magnitude data corresponding to sites with latitude 60N and above
filter.gc(rate2015,lat.low=60)
filter.gc(magn2015,lat.low=60)
}