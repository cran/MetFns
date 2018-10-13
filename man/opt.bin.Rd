\name{opt.bin}
\alias{opt.bin}
\title{
Divides data in blocks of optimal bin sizes
}
\description{Divides given rate or magnitude data into blocks of optimal bin sizes, for specified shower, period of days, minimum and maximum bin sizes, number of meteors.
}
\usage{
opt.bin(data,date.start,date.end,shw,kmin=0.01,kmax=1,num)
}
\arguments{
  \item{data}{
  data frame consisting of visual meteor rate data.
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
  numeric vector specifying total number of meteors per interval. Minimum accepted value is 1.
}
}
\details{
Data is divided into blocks using optimal bin size algorithm. For each block of data, it searches for an optimal bin size between minimum bin size \code{kmin} and maximum bin size \code{kmax} with total number of meteors per bin \code{num}. If there are not enough meteors, a maximum bin size is used. For further calculations, only observing intervals with lengths smaller or equal to optimal bin are used.}
\value{
List containing blocks of rate or magnitude data of optimal bin sizes.
}

\author{
Kristina Veljkovic
}
\seealso{
\code{\link{zhr}},\code{\link{pop.index2}}
}
\examples{
## Divide 2015 Orionids rate data into blocks of optimal sizes.
## Consider time period around the maximum, 21th to 25th October, 
## min bin size 0.16 degrees (4 hours), max bin size 1 degree, 
## number of meteors equals 100

## First select rate data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, 
## percentage of clouds below 20
ori2015<-filter(rate2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
P.up=20,mag.low=5,mag.up=8,h.low=20)
opt.bin(ori2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
kmin=0.16,kmax=1,num=100)
}




