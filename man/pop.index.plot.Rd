\name{pop.index.plot}
\alias{pop.index.plot}
\title{
Plot of population index
}
\description{Plots population index of a meteor shower
for data containing population index values and given limits on x and y axes.
}
\usage{
pop.index.plot(rdata,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
}
\arguments{
  \item{rdata}{
 data frame consisting of population index values.
}
\item{xlim1}{
  numeric vector specifying minimum value on x-axis.
}
\item{xlim2}{
  numeric vector specifying maximum value on x-axis.
}
\item{xinc}{
  numeric vector specifying increment between labels on x-axis.
}
\item{ylim1}{
  numeric vector specifying minimum value on y-axis.
}
\item{ylim2}{
  numeric vector specifying maximum value on y-axis.
}
\item{yinc}{
  numeric vector specifying increment between labels on y-axis.
}
}
\details{
\code{rdata} represents table of population index values calculated using functions \code{pop.index} or \code{pop.index2}.
}
\value{
xy plot of population index is made. Solar longitude is on x-axis and population index on y-axis. Population index is represented with black filled circles and \emph{68\%} confidence intervals.
Values of limits (minimum and maximum values) on x and y axis (\code{xlim1,xlim2,ylim1,ylim2}), as well as increments between the axis labels
(\code{xinc,yinc}) should be provided to function call.
}
\references{
Arlt, R. (2003). Bulletin 19 of the International Leonid Watch: Population index study of the 2002 Leonid meteors. \emph{WGN, Journal of the IMO},31:3, 77-87.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index}},\code{\link{pop.index2}}
}
\examples{
## calculate and make plot of population index for observations of 2015 Orionids, time period 
## around maximum, 21th to 25th October, min bin size 0.1 degree, max bin size 1 degrees, 
## number of meteors equals 100 
## First select magnitude data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
ori2015mag<-filter(magn2015,date.start="2015-10-21",date.end="2015-10-25",
shw="ORI",P.up=20,mag.low=5,h.low=20)
oripop<-pop.index2(ori2015mag,date.start="2015-10-21",date.end="2015-10-25",
shw="ORI",kmin=0.1,kmax=1,num=100)
pop.index.plot(oripop,xlim1=207,xlim2=211,xinc=1,ylim1=1.5,ylim2=2.5,yinc=0.2)

}

