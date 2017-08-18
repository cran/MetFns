\name{filter.totcor}
\alias{filter.totcor}
\title{
Selection of visual meteor data by total correction factor
}
\description{
Selects data for a given visual meteor rate dataset, specified shower, population index and total correction factor.
}
\usage{
filter.totcor(data,shw,r,C = 5)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
  \item{r}{
numeric vector specifying population index of a meteor shower.
}
  \item{C}{
numeric vector specifying total correction factor. C=5 is set as a default value.
}
}
\details{
Total correction factor accounts for all non-ideal observing conditions 
such as clouds, low radiant, low limiting magnitude.

Total correction factor is equal to \eqn{C=\frac{F r^{(6.5-lmg)}}{\sin h}}{C=F*r^(6.5-lmg)\ sin(h)}, where \eqn{r} is population index, \eqn{lmg} limiting magnitude, \eqn{F} correction factor for field-of-view obstruction, \eqn{h} radiant elevation.

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used.
}
\value{
\code{filter.totcor} returns data frame containing observations with total correction factors upper bounded (smaller or equal)
by argument \code{C}
}
\references{
Rendtel J. and Arlt R., editors (2014). \emph{Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "Latitude", "Longitude", "F","Lmg" and "Shower".
}
\seealso{
\code{\link{filter}},\code{filter.h},\code{\link{filter.mag}},\code{\link{filter.P}},\code{\link{zhr}}
}
\examples{
## select 2015 visual meteor rate data during the period between 10-25 October and then
## select observations of Orionids with total correction factor smaller or equal to 5
rateOct<-filter.date(rate2015,date.start="2015-10-10",date.end="2015-10-25")
filter.totcor(rateOct,shw="ORI",r=2.5)
}

