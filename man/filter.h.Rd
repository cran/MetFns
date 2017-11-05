\name{filter.h}
\alias{filter.h}
\title{
Selection of visual meteor data by radiant elevation
}
\description{
Selects data for a given visual meteor dataset, specified shower and its radiant elevation or interval of radiant elevations.
}
\usage{
filter.h(data,shw,h.low = 0,h.up = 90)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
  \item{h.low}{
numeric vector with value between 0 (default) and 90, specifying lower boundary of radiant elevation in degrees.
}
  \item{h.up}{
numeric vector with value between 0 and 90 (default), specifying upper boundary of radiant elevation in degrees.
}
}
\details{
Radiant elevation \emph{h} is measured from the observer's horizon towards zenith. 

Values from the data frame
\code{\link{radiant}} are used for right ascension and declination of shower radiant.
}
\value{
\code{filter.h} returns data frame containing observations  corresponding to the radiant elevation between 
\code{h.low} and \code{h.up}.
}
\references{
Rendtel J. and Arlt R., editors (2014). \emph{Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "Shower", "Start.Date", "End.Date", "Longitude"
and "Latitude".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor rate data for the elevation of Orionids radiant between 20 and 90 degrees,
## period between 10-25th October 2015
rateOri<-filter.date(rate2015,"2015-10-10","2015-10-25")
filter.h(rateOri,shw="ORI", h.low=20)
}