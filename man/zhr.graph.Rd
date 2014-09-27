\name{zhr.graph}
\alias{zhr.graph}
\title{
Graphic of  zenithal hourly rate (ZHR)
}
\description{Graphical representation of average zenithal hourly rate of a meteor shower with error bars
for a given rate dataset, specified shower, period of days, population index, length of time interval, ZHR correction 
and type of x-axis display.
}
\usage{
zhr.graph(data,year,month, day.beg, day.end=day.beg,shw, r=NULL, Ralpha=NULL, 
          Delta=NULL, k,c=1,type=c("UTC","sol"))
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{year}{
numeric vector of length 4 specifying year.
}
   \item{month}{
numeric vector specifying month of the year.
}
   \item{day.beg}{
numeric vector specifying beginning day.
}
   \item{day.end}{
numeric vector specifying ending day.
}
\item{shw}{
character string consisting of three capital letters which represent meteor shower code.
}
\item{r}{
numeric vector specifying population index of a meteor shower.
}
\item{Ralpha}{
numeric vector with value between 0 and 360, specifying right ascension of the radiant, in degrees.
}
 \item{Delta}{
numeric vector with value between -90 and +90, specifying declination of the radiant, in degrees.
} 
  \item{k}{
 numeric vector specifying length of time interval in hours.
}
 \item{c}{
 numeric vector specifying value of ZHR correction. By default, c=1.
}
 \item{type}{
 character string taking a value "UTC" or "sol", specifying the notation on x-axis of the graphic.
}
}
\details{
For type="UTC", tick marks on x-axis represent coordinated universal time (UTC), set \emph{k} distance apart, 
with labels specifying date (at 00:00 UTC). For type="sol", tick marks and labels on x-axis represent solar longitude,
corresponding to above mentioned time in UTC.
}
\value{
xy plot of Zenithal Hourly Rate, with time (UTC) or solar longitude on x-axis and ZHR on y-axis. ZHR is represented with
black filled circles with \emph{68\%} confidence intervals/one sigma error bars.
}
\references{
\url{http://www.imo.net}
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{zhr}}
}
\examples{
## select data for observations of Orionids, period 16-30th October 2006, 
## 12hrs time intervals, and make a graphic of ZHR 
data(rate06)
require(plotrix)
rateOri<-filter(rate06,shw="ORI",year=2006, month=10, day.beg=16, day.end=30)
zhr.graph(rateOri,year=2006,month=10,day.beg=16,day.end=30,shw="ORI",r=2.5,k=12,type="UTC")
}




