\name{filter.h}
\alias{filter.h}
\title{
Selection of visual meteor data by radiant elevation
}
\description{
Selects data for a given visual meteor dataset, specified shower and its radiant elevation or interval of radiant elevations.
}
\usage{
filter.h(data,shw,Ralpha = NULL,Delta = NULL,h.low = 10,h.up = 90)
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
  \item{h.low}{
numeric vector with value between 0 and 90, specifying lower boundary of radiant elevation in degrees. Default value is 10.
}
  \item{h.up}{
numeric vector with value between 0 and 90 (default), specifying upper boundary of radiant elevation in degrees.
}
}
\details{
Radiant elevation \emph{h} is measured from the observer's horizon towards zenith. 

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used.
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