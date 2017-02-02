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
filter(data,year = NULL,month.beg = NULL,month.end=month.beg,day.beg = NULL,
day.end = day.beg,time.beg=0,time.end=2359,shw = NULL,imocode = NULL,
long.low = 0,long.up = 180,ew = c("E", "W"),lat.low = 0,lat.up = 90,
ns = c("N", "S"),name = NULL,fname = NULL,site = NULL,country = NULL,
mag.low = 3,mag.up = 8,F.low = 1,F.up = 3,sol.low = 0,sol.up = 360,
Ralpha = NULL,Delta = NULL,h.low = 10,h.up = 90,r = NULL,C = 5)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{year}{
numeric vector of length 4 specifying year.
}
  \item{month.beg}{
numeric vector specifying the beginning month.
}
  \item{month.end}{
numeric vector specifying the ending month. By default, \code{month.end} is set to be equal to \code{month.beg}.
}
  \item{day.beg}{
numeric vector specifying the beginning day.
}
  \item{day.end}{
numeric vector specifying the ending day. By default, \code{day.end} is set to be equal to \code{day.beg}.
}
  \item{time.beg}{
numeric vector (0-2359) specifying lower boundary of time in hours and minutes, corresponding to \code{day.beg}. By default, \code{time.beg} is set to be equal to 0.
}
  \item{time.end}{
numeric vector(0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
  \item{imocode}{
character string consisting of five capital letters which represent IMO observer code.
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
  \item{name}{
character string representing observer's last name.
}
  \item{fname}{
character string representing observer's first name.
}
  \item{site}{
character string specifying name of the observing site.
}
  \item{country}{
character string specifying name of the country.
}
  \item{mag.low}{
numeric vector with value between 3.0 (default) and 8.0, specifying lower 
boundary of limiting magnitude.
}
  \item{mag.up}{
numeric vector with value between 3.0 and 8.0 (default), specifying upper 
boundary of limiting magnitude.
}
  \item{F.low}{
numeric vector with value between 1.0 (default) and 3.0, specifying lower 
boundary of correction factor for field-of-view obstruction.
}
  \item{F.up}{
numeric vector with value between 1.0 and 3.0 (default), specifying upper 
boundary of correction factor for field-of-view obstruction.
}
  \item{sol.low}{
numeric vector with value between 0 (default) and 360, specifying lower 
boundary of solar longitude in degrees.
}
  \item{sol.up}{
numeric vector with value between 0 and 360 (default), specifying upper 
boundary of solar longitude in degrees.
}
  \item{Ralpha}{
numeric vector with value between 0 and 360, specifying right ascension of the 
radiant, in degrees.
}
  \item{Delta}{
numeric vector with value between -90 and +90, specifying declination of the 
radiant, in degrees.
}
  \item{h.low}{
numeric vector with value between 10 (default) and 90, specifying lower boundary 
of radiant elevation in degrees.
}
  \item{h.up}{
numeric vector with value between 10 and 90 (default), specifying upper boundary 
of radiant elevation in degrees.
}
  \item{r}{
numeric vector specifying population index of a meteor shower.
}
  \item{C}{
numeric vector specifying total correction factor. C=5 is set as a default value.
}
}
\details{
Depending on the given arguments, the function \code{filter} calls one or more 
particular filters for selection of visual meteor data.
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{filter.shw}}, \code{\link{filter.datetime}}, \code{\link{filter.imocode}},
\code{\link{filter.obsname}}, \code{\link{filter.site}}, 
\code{\link{filter.country}}, \code{\link{filter.gc}}, \code{\link{filter.mag}}, \code{\link{filter.F}},
\code{\link{filter.sol}}, \code{\link{filter.h}}, \code{\link{filter.totcor}}   
}
\examples{
## select rate and magnitude data for observations of 1997 Perseids, time period 28th July
## to 16th August, radiant elevation higher than 20 degrees, 
## total correction factor smaller than 5(default)
data(rate97)
filter(rate97,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,shw="PER",h.low=20)

data(magn97)
filter(magn97,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,shw="PER",h.low=20)
}
