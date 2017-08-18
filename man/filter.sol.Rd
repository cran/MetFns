\name{filter.sol}
\alias{filter.sol}
\title{
Selection of visual meteor data by solar longitude
}
\description{
Selects data for a given visual meteor dataset and specified interval of solar longitudes.
}
\usage{
filter.sol(data,sol.low,sol.up)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{sol.low}{
numeric vector specifying lower boundary of solar longitude in degrees.
}
  \item{sol.up}{
numeric vector specifying upper boundary of solar longitude in degrees.
}
}
\details{
The function \code{filter.sol} selects data with solar longitudes bounded by \code{sol.low} and \code{sol.up}.  In selection of the data, solar
longitude corresponding to the middle of the observing time period is used.}
\value{
\code{filter.sol} returns data frame with the same number of columns as the argument \code{data},
containing observations with solar longitudes between \code{sol.low} and \code{sol.up}.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "Sollong".
}
\seealso{
\code{\link{filter}},\code{\link{solar.long}}
}
\examples{
## select 2015 rate and magnitude data with solar longitudes between 191 and 222 degrees
filter.sol(rate2015,sol.low=191,sol.up=222)
filter.sol(magn2015,sol.low=191,sol.up=222)
}