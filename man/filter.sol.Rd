\name{filter.sol}
\alias{filter.sol}
\title{
Selection of visual meteor data by solar longitude
}
\description{
Selects data for a given visual meteor dataset and specified solar longitude or interval of solar longitudes.
}
\usage{
filter.sol(data,sol.low = 0,sol.up = 360)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{sol.low}{
numeric vector with value between 0 (default) and 360, specifying lower boundary of solar longitude in degrees.
}
  \item{sol.up}{
numeric vector with value between 0 and 360 (default), specifying upper boundary of solar longitude in degrees.
}
}
\value{
\code{filter.sol} returns data frame with the same number of columns as the argument \code{data},
containing observations with solar longitudes between \code{sol.low} and \code{sol.up}.
}
\references{
\url{http://www.imo.net/data/visual}

Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "year" and "sollong".
}
\seealso{
\code{\link{filter}},\code{\link{solar.long}}
}
\examples{
## select visual meteor data for 2005. with solar longitudes between 200 and 215 degrees

## rate data for the year 2005
data(rate05)
filter.sol(rate05,sol.low=200,sol.up=215)

## magnitude data for the year 2005
data(magn05)
filter.sol(magn05,sol.low=200,sol.up=215)
}