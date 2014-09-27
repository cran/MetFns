\name{filter.time}
\alias{filter.time}
\title{
Selection of visual meteor data by time period
}
\description{
Selects data for a given visual meteor dataset and specified time period.
}
\usage{
filter.time(data, time.low = 0, time.up = 2359)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{time.low}{
numeric vector (0-2359) specifying lower boundary of time in hours and minutes.
}
  \item{time.up}{
numeric vector(0-2359) specifying upper boundary of time in hours and minutes.
}
}
\value{
\code{filter.time} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to the period of time bounded by \code{time.low} and \code{time.up}.
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
Argument \code{data} has to consist of the columns named "start" and "stop".
}
\seealso{
\code{\link{filter}}, \code{\link{filter.date}}
}
\examples{
## select visual meteor data for the 12th August 2007, from 2100-2359 UTC.

## rate data for the year 2007
data(rate07)
rate1208<-filter.date(rate07,year=2007,month=8,day.beg=12)
filter.time(rate1208,time.low=2100,time.up=2359)

## magnitude data for the year 2007
data(magn07)
magn1208<-filter.date(magn07,year=2007,month=8,day.beg=12)
filter.time(magn1208,time.low=2100,time.up=2359)
}
