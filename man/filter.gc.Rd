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
filter.gc(data,long.low = 0,long.up = 180,ew = c("E", "W"),lat.low = 0,
lat.up = 90,ns = c("N", "S"))
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{long.low}{
numeric vector taking a value between 0 (default) and 180, specifying lower boundary of longitude in degrees.
}
  \item{long.up}{
numeric vector taking a value between 0 and 180 (default), specifying upper boundary of longitude in degrees.
}
  \item{ew}{
character vector (E,W), specifying east or west position from the prime meridian.
}
  \item{lat.low}{
numeric vector taking a value between 0 (default) and 90, specifying lower boundary of latitude in degrees.
}
  \item{lat.up}{
numeric vector taking a value between 0 and 90 (default), specifying upper boundary of latitude in degrees.
}
  \item{ns}{
character vector (N,S), specifying north or south position from equator.
}
}
\details{
If values of arguments \code{long.low} and \code{long.up}, as well as \code{lat.low} and \code{lat.up}, are the same,
\code{filter.gc} selects data for  particular observing site.

\code{filter.gc} enables one to select data only by longitude or latitude, with geographical coordinates
being between given boundaries, less, greater or equal to a boundary.
}
\value{
\code{filter.gc} returns data frame with the same number of columns as the argument \code{data},
containing observations corresponding to geographical coordinates with longitude between \code{long.low} and \code{long.up} 
and latitude between \code{lat.low} and \code{lat.up}.
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "long", "EW", "lat" and "NS".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select visual meteor data for 2004, site with longitude 19.7E and latitude 44.2N

## rate data for the year 2004
data(rate04)
filter.gc(rate04,long.low=19.7,long.up=19.7,ew="E",lat.low=44.2,lat.up=44.2,ns="N")

## magnitude data for the year 2004
data(magn04)
filter.gc(magn04,long.low=19.7,long.up=19.7,ew="E",lat.low=44.2,lat.up=44.2,ns="N")


## select visual meteor data corresponding to sites with latitude 44.2N and above
filter.gc(rate04,lat.low=44.2,ns="N")
filter.gc(magn04,lat.low=44.2,ns="N")
}