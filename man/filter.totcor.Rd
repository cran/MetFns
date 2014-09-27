\name{filter.totcor}
\alias{filter.totcor}
\title{
Selection of visual meteor data by total correction factor
}
\description{
Selects data for a given visual meteor rate dataset, specified shower, population index and total correction factor.
}
\usage{
filter.totcor(data, shw, Ralpha = NULL, Delta = NULL, r, C = 5)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
  \item{Ralpha}{
numeric vector with value between 0 and 360, specifying right ascension of the radiant, in degrees.
}
  \item{Delta}{
numeric vector with value between -90 and +90, specifying declination of the radiant, in degrees.
}
  \item{r}{
numeric vector specifying population index of a meteor shower.
}
  \item{C}{
numeric vector specifying maximum correction factor. C=5 is set as a default value.
}
}
\details{
Correction factor accounts for all non-ideal observing conditions 
such as clouds, low radiant, low limiting magnitude.

Correction factor is equal to \emph{C=r^(6.5-lmg)F/sin(h)}, where \emph{r} is population index, \emph{lmg} limiting magnitude, 
\emph{F} correction factor for clouds, \emph{h} radiant elevation.

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used.
}
\value{
\code{filter.correct} returns data frame containing observations with  correction factors upper bounded 
by argument \code{C}.
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
Argument \code{data} has to consist of the columns named "SPO" (placed before the columns for showers), 
"long", "EW", "lat", "day", "month", "year", "lmg" and "F".
}
\seealso{
\code{\link{filter}},\code{\link{filter.F}},\code{\link{filter.mag}},\code{\link{zhr}}
}
\examples{
## select visual meteor data for the period between 15-30 October 2006 and then
## select observations of Orionids with maximum correction factor equal to 5

## rate data for the year 2006
require(astroFns)
data(rate06)
rateOri<-filter.date(rate06,year=2006,month=10,day.beg=15,day.end=30)
filter.totcor(rateOri,shw="ORI",r=2.5)
}

