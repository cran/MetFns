\name{filter}
\alias{filter}
\title{
Global filter
}
\description{
Various data selections for a given visual meteor data. Wrapper function for filters by shower code, date, time period, 
IMO observer code, observer's name, geographical coordinates, site, country, limiting magnitude, correction 
factor for field-of-view obstruction, solar longitude, radiant elevation and total correction factor.
}
\usage{
filter(data,date.start=NULL,date.end=NULL,shw=NULL,lat.low=-90,lat.up=90,
long.low=-180,long.up=180,fname=NULL,lname=NULL,site=NULL,country=NULL,
mag.low=1,mag.up=8,P.low=0,P.up=90,sol.low=NULL,sol.up=NULL,
h.low=0,h.up=90,r=NULL,C=NULL)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{date.start}{
character vector or factor specifying start date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
  \item{date.end}{
character vector or factor specifying end date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). 
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
  \item{lat.low}{
numeric vector taking a value between -90 (default) and 90, specifying lower boundary of latitude in degrees.
}
  \item{lat.up}{
numeric vector taking a value between -90 and 90 (default), specifying upper boundary of latitude in degrees.
}
  \item{long.low}{
numeric vector taking a value between -180(default) and 180, specifying lower boundary of longitude in degrees.
}
  \item{long.up}{
numeric vector taking a value between -180 and 180(default), specifying upper boundary of longitude in degrees.
}
  \item{fname}{
character string specifying observer's first name.
}
  \item{lname}{
character string specifying observer's last name.
}
  \item{site}{
character string specifying name of the observing site.
}
  \item{country}{
character string specifying name of the observing country.
}
  \item{mag.low}{
numeric vector with value between 1(default) and 8, specifying lower boundary of limiting magnitude.
}
  \item{mag.up}{
numeric vector with value between 1 and 8(default), specifying upper boundary of limiting magnitude.
}
  \item{P.low}{
numeric vector with value between 0 (default) and 90, specifying lower boundary of percentage of field-of-view obstruction. 
}
  \item{P.up}{
numeric vector with value between 0 and 90(default), specifying upper boundary of percentage of field-of-view obstruction.
}
  \item{sol.low}{
numeric vector with value between 0 and 360, specifying lower 
boundary of solar longitude in degrees.
}
  \item{sol.up}{
numeric vector with value between 0 and 360, specifying upper 
boundary of solar longitude in degrees.
}
  \item{h.low}{
numeric vector with value between 0(default) and 90, specifying lower boundary 
of radiant elevation in degrees.
}
  \item{h.up}{
numeric vector with value between 0 and 90(default), specifying upper boundary 
of radiant elevation in degrees.
}
  \item{r}{
numeric vector specifying population index of a meteor shower.
}
  \item{C}{
numeric vector specifying total correction factor. 
}
}
\details{
Depending on the given arguments, the function \code{filter} calls one or more 
particular filters for selection of visual meteor data.
}
\references{
Rendtel J. and Arlt R., editors (2014). \emph{Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{filter.shw}}, \code{\link{filter.date}}, \code{\link{filter.obsname}}, \code{\link{filter.site}}, 
\code{\link{filter.country}}, \code{\link{filter.gc}}, 
\code{\link{filter.mag}}, \code{\link{filter.P}},\code{\link{filter.sol}}, \code{\link{filter.h}}, \code{\link{filter.totcor}}   
}
\examples{
## select rate data for Orionids activity from 10th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
filter(rate2015,date.start="2015-10-10",date.end="2015-10-25",shw="ORI",P.up=20,mag.low=5,h.low=20)
}
