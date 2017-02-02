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
data frame consisting of visual meteor data (rate or magnitude data).
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
numeric vector with value between 10 (default) and 90, specifying lower boundary of radiant elevation in degrees.
}
  \item{h.up}{
numeric vector with value between 10 and 90 (default), specifying upper boundary of radiant elevation in degrees.
}
}
\details{
Radiant elevation \emph{h} is measured from the observer's horizon towards zenith and it takes value
between 10 and 90 degrees.

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used.
}
\value{
\code{filter.h} returns data frame containing observations  corresponding to the radiant elevation between 
\code{h.low} and \code{h.up}.
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "SPO" in rate data frames (placed before the columns for showers) 
and columns "zero" and "Shw" in magnitude data frames.

Also, \code{data} has to consist of the columns named "long", "EW", "lat", "day", "month", "year".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor data for the elevation of Perseids radiant between 30 and 60 degrees,
## period between 1-20th August 2011

## rate data for the year 2011
data(rate11)
ratePer<-filter.datetime(rate11,year=2011,month.beg=8,day.beg=1,day.end=20)
filter.h(ratePer,shw="PER", h.low=30,h.up=60)

## magnitude data for the year 2011 
data(magn11)    
magnPer<-filter.datetime(magn11,year=2011,month.beg=8,day.beg=1,day.end=20)
filter.h(magnPer,shw="PER",h.low=30,h.up=60)
}