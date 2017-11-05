\name{pop.index2}
\alias{pop.index2}
\title{
Calculation of population index based on average distance from the limiting magnitude
}
\description{Calculates population index of a meteor shower
for a given magnitude data, specified time period, minimum and maximum bin size, and number of meteors.
}
\usage{
pop.index2(data,date.start,date.end,shw,kmin=0.01,kmax=1,num) 
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
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
   \item{kmin}{
  numeric vector specifying minimum bin size, in degrees of solar longitude. By default, it is equal to 0.01. Minimum accepted value is 0.001.
}
  \item{kmax}{
  numeric vector specifying maximum bin size, in degrees of solar longitude. By default, it is equal to 1. Maximum accepted values is 5.
}
\item{num}{
  numeric vector specifying total number of meteors per interval.
}
}
\details{Optimal bin size algorithm is used. It searches for an optimal bin size between minimum bin size \code{kmin} and maximum bin size \code{kmax} with total number of meteors per bin \code{num}. If there are not enough meteors, a maximum bin size is used. For further calculations, only observing intervals with lengths smaller or equal to optimal bin are used. 

Average distance from the limiting magnitude, as difference between the limiting magnitude and average meteor magnitude, is calculated for each observing time interval. Final average distance from the limiting magnitude is calculated as a weighted average of all individual average distances, where numbers of meteors in each observing interval represent weights. Conversion of average distance from limiting magnitude to population index is done using natural spline interpolation \code{spline} of table values 
\code{\link{popind}}. 
Error margins of population index are calculated using bilinear interpolation \code{interp2} of table values 
\code{\link{popind.err}}. 

If total number of meteors per bin is smaller than 10, NA values
are returned for population index and its standard error. For the case when total number of meteors is greater than 9369, polynomial regression of second degree is used in calculation of standard error of population index.   
}
\value{
Data frame containing following vectors
\describe{
   \item{sollong}{numeric Mean of observers' solar longitudes weighted by \eqn{\frac{N_{obs}}{C_{obs}}}{N_{obs}/C_{obs}}, observers' numbers of meteors divided by total correction factor}
   \item{date}{POSIXct  Calendar date and time in UTC corresponding to \code{sollong}}
   \item{nINT}{numeric Number of observing time intervals}
   \item{nSHW}{numeric Number of observed meteors belonging to the shower}
   \item{pop.index}{numeric Population index}
   \item{r.error}{numeric Error margins of population index}
}
}
\references{
Arlt, R. (2003). Bulletin 19 of the International Leonid Watch: Population index study of the 2002 Leonid meteors. \emph{WGN, Journal of the IMO},31:3, 77-87.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index}},\code{\link{pop.index.plot}}
}
\examples{
## calculate population index for observations of 2015 Orionids, time period 
## around maximum, 21th to 25th October, min bin size 0.1 degree, max bin size 1 degrees, 
## number of meteors equals 100 
## First select magnitude data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
ori2015mag<-filter(magn2015,date.start="2015-10-21",date.end="2015-10-25",
shw="ORI",P.up=20,mag.low=5,h.low=20)
pop.index2(ori2015mag,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
kmin=0.1,kmax=1,num=100)

}








