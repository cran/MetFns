\name{pop.index.plot}
\alias{pop.index.plot}
\title{
Plot of population index
}
\description{Plots population index of a meteor shower
for data containing population index values and given limits on x and y axes.
}
\usage{
pop.index.plot(data,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
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
## 10th to 25th October, min bin size 0.5 degree, max bin size 2 degrees, 
## number of meteors equals 100 
## First select magnitude data for Orionids activity from 10th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
ori2015mag<-filter(magn2015,date.start="2015-10-10",date.end="2015-10-25",
shw="ORI",P.low=0,P.up=20,mag.low=5,mag.up=8,h.low=20,h.up=90)
oripop<-pop.index2(ori2015mag,date.start="2015-10-10",date.end="2015-10-25",
shw="ORI",kmin=0.5,kmax=2,num=100)
pop.index.plot(oripop,xlim1=197,xlim2=211,xinc=1,ylim1=1.4,ylim2=3.4,yinc=0.2)

}

